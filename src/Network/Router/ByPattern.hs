{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Router.ByPattern
  ( newRouter
  )
where

import           Control.Monad
import qualified Data.ByteString.Lazy          as L
--import           Data.Pattern.Haskell
import           Data.Pattern.Matcher.Haskell
import           Data.Pattern.Matcher
import           Data.Pattern.Types
import qualified ListT                         as T
import           Network.Router.Types
import           Network.Router.Util
import           Network.Top.Util
import qualified StmContainers.Multimap        as SMM
import           System.Timeout

instance Hashable v => Hashable (Match v)

--instance Hashable v => Hashable (Pattern v)
-- instance Hashable WildCard
-- |Channels, retrievable by their type
type State = SMM.Multimap AbsType Entry

newState :: IO State
newState = SMM.newIO

entries = atomically . T.toList . SMM.listT

entriesByType st t = atomically . T.toList $ SMM.listTByKey t st

data Entry =
  Entry
    { eClient  :: Client
    , ePattern :: Pattern
    , eMatcher :: L.ByteString -> Bool
    }

-- eClient is enough as in this context is a unique key
instance Eq Entry where
  (==) e1 e2 = eClient e1 == eClient e2

instance Hashable Entry where
  hashWithSalt salt e = salt `hashWithSalt` eClient e -- `hashWithSalt` ePattern e

instance Show Entry where
  show e = unwords ["ByPattern" ++ (show . eClient $ e), show . ePattern $ e]

-- ByPattern need to know the definition of the type references by the patterns
-- so it depends on the Repo service and/or its local data type definitions cache
newRouter msgBus solver = do
  state <- newState
  bus   <- onTypedMsg
    msgBus
    "Pattern"
    (\msgType msgBody -> forward (const True) state msgType msgBody)
  return $ router (Proxy :: Proxy (ByPattern ()))
                  (handle state bus solver)
                  (reportTxt state)
                  (reportTyped state)
 where
  reportTyped state = do
    r <-
      typedBLOB
      .   ByPatternReport
      .   map (\(t, e) -> (t, ePattern e, asClientReport . eClient $ e))
      <$> entries state
    return $ NestedReport "ByPattern" r []
  reportTxt = report__ "ByPattern" entries
  report__ name sel state = report
    (name ++ " Router")
    [("Open Connections:", bulletList . map (p . show) <$> sel state)]
  handle st bus solver [t] bs = do
    let ByPattern pat :: ByPattern () = decodeOK bs
    dbgS "ByPattern solving"
      --eme ::  Either SomeException (Maybe (Either String AbsTypeModel))<- try $ timeout (seconds 11) $ solver t
    ev <- join <$> withTimeout 11 (solver t)
    dbg ["ByPattern solved", take 100 $ show ev]
    case ev of
      Left err -> return . Left $ unwords
        ["Cannot locate definition of", show t, show err]
        --Right me -> case me of
          --Nothing -> return . Left $ unwords ["Cannot locate definition of",show t]
          --Just e -> case e of
          --  Left err -> return . Left $ unwords ["Cannot locate definition of",show t,show err]
      Right tm -> do
        let chk = matchPM (patternMatcher tm pat) . L.toStrict
        let ef conn = Entry conn pat chk
        return . Right $ route ef t bus st
  route ef t bus st myConn = do
    let me = ef myConn
    liftIO $ dbg ["Protocol ByPattern", show t, show me]
    atomically $ SMM.insert me t st
    let source = eClient me
      -- on failure remove conn from map and terminate thread
    loop (atomically $ SMM.delete me t st) $ do
      msg <- fromClient source
        --dbg ["IN",show t,show source,show . L.unpack $ msg]
      forward (\dest -> source /= eClient dest) st t msg
      output bus (TypedMsg t msg)
  forward sel st t msg = do
    destinations <-
      filter (\dest -> sel dest && eMatcher dest msg) <$> entriesByType st t
      --dbg ["TO",show t,show destinations]
    mapM_ (\e -> toClient (eClient e) msg) $ destinations
