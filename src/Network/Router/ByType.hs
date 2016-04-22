{-# LANGUAGE ScopedTypeVariables #-}
module Network.Router.ByType where

import Network.Router.Util
import Network.Router.Types

import qualified Data.ByteString.Lazy   as L
import qualified ListT                                as T
import qualified STMContainers.Set                    as S
import qualified STMContainers.Multimap               as SMM
import Data.Bifunctor

-- instance Hashable ByType

type ByTypeState = SMM.Multimap AbsType Client -- Multiple clients per hub

newByTypeRouter = do
  state <- SMM.newIO
  return $ router (Proxy::Proxy (ByType ())) (handler_ state) (report_ state) (reportTyped state)

    where

      reportTyped state = do
         r <- typedBytes . ByTypeReport . map (second asClientReport) <$> allConns state
         return $ NestedReport "ByType" r []

      report_ state = report "ByType Router" [
        ("Open Connections"
        ,bulletList . map (p . show) <$> allConns state
        )]

      allConns = atomically . T.toList . SMM.stream

      handler_ :: ByTypeState -> AbsType -> [Word8] -> Client -> IO ()
      handler_ st t bs client = do
        let ByType :: ByType () = decodeOK bs
        liftIO $ dbg ["Protocol",show t,show client]
        atomically $ SMM.insert client t st
        loop (atomically $ SMM.delete client t st) $ do
          msg <- fromClient client
          dbg [show t,"IN",show . L.unpack $ msg]
          -- send to all but the message sender
          cs <- filter (/= client) <$> (atomically $ T.toList $ SMM.streamByKey t st)
          dbg [show t,"TO",show cs]
          mapM_ (\c -> toClient c msg) cs
