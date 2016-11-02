{-# LANGUAGE ScopedTypeVariables #-}
module Network.Router.ByType where

import           Data.Bifunctor
import qualified Data.ByteString.Lazy   as L
import           Data.Maybe
import           Data.Pattern.Haskell
import           Data.Pattern.Types
import qualified ListT                  as T
import           Network.Router.Types
import           Network.Router.Util
import           Network.Top
import qualified STMContainers.Multimap as SMM
import qualified STMContainers.Set      as S

instance Hashable v => Hashable (Pattern v)
instance Hashable WildCard

type ByTypeState = SMM.Multimap AbsType Entry

data Entry = Entry {
  eClient   ::Client
  ,ePattern ::Maybe (Pattern WildCard)
  ,eAccept  ::Client -> ByteString -> Bool -- return True if this Entry accepts messages from given client and with given content
  }

instance Hashable Entry where hashWithSalt salt e =  salt `hashWithSalt` ePattern e `hashWithSalt` eClient e

instance Show Entry where
  show c | ePattern c == Nothing = "ByType" ++ show (eClient c)
         | otherwise             = unwords ["ByPattern"++show (eClient c),show . fromJust . ePattern $ c]

-- eClient should be enough
instance Eq Entry where (==) e1 e2 = eClient e1 == eClient e2 && ePattern e1 == ePattern e2

newByTypeRouters = do
  state <- SMM.newIO
  return [router (Proxy::Proxy (ByType ())) (handler_ False state) (report_ False state) (reportTyped False state)
         ,router (Proxy::Proxy (ByPattern ())) (handler_ True state) (report_ True state) (reportTyped True state)]
    where

      reportTyped False state = do
         r <- typedBLOB . ByTypeReport . map (second (asClientReport . eClient)) <$> byTypeConns state
         return $ NestedReport "ByType" r []

      reportTyped True state = do
         r <- typedBLOB . ByPatternReport . map (\(t,e) -> (t,fromJust . ePattern $ e,asClientReport . eClient $ e)) <$> byPatternConns state
         return $ NestedReport "ByPattern" r []

      report_ False = report__ "ByType" byTypeConns
      report_ True  = report__ "ByPattern" byPatternConns

      report__ name sel state =  report (name++" Router") [
        ("Open Connections:"
        ,bulletList . map (p . show) <$> sel state
        )]

      byTypeConns = conns isNothing
      byPatternConns = conns isJust
      conns sel state = filter (sel . ePattern . snd) <$> (atomically . T.toList . SMM.stream) state

      -- one thread per conn
      -- handler_ :: Bool -> ByTypeState -> [AbsType] -> ByteString -> Client -> IO ()
      handler_ False st _ [t] bs = try $ do
        let ByType :: ByType () = decodeOK bs
        -- send to all but the message sender, we cannot check the type as that would require getting its definition
        let ef = \myConn -> (Entry myConn Nothing (\source _ -> source /= myConn))
        return $ route ef t st

      handler_ True st solver [t] bs = try $ do
        let ByPattern pat :: ByPattern (Pattern WildCard) = decodeOK bs
        eenv <- solver t
        case eenv of
          Left err -> error $ unwords ["Cannot locate definition of",show t]
          Right env -> do
            let Right pp = envPattern env pat
            let chk = match (matcher pp)
            let ef = \myConn ->  Entry myConn (Just pat) (\source msg -> source /= myConn && chk (L.toStrict msg))
            -- let e = Entry myConn (Just pat) (\source msg -> chk (L.toStrict msg))
            return $ route ef t st

      route ef t st myConn = do
        let e = ef myConn
        liftIO $ dbg ["Protocol",show t,show e]
        atomically $ SMM.insert e t st
        let source = eClient e
        -- on failure remove conn from map and terminate thread
        loop (atomically $ SMM.delete e t st) $ do
          msg <- fromClient source
          --dbg ["IN",show t,show source,show . L.unpack $ msg]
          destinations <- filter (\dest -> eAccept dest source msg) <$> (atomically $ T.toList $ SMM.streamByKey t st)
          --dbg ["TO",show t,show destinations]
          mapM_ (\e -> toClient (eClient e) msg) $ destinations
