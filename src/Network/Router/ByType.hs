{-# LANGUAGE ScopedTypeVariables #-}
module Network.Router.ByType where

import Network.Router.Util
import Network.Router.Types

import qualified Data.ByteString.Lazy   as L
import qualified ListT                                as T
import qualified STMContainers.Set                    as S
import qualified STMContainers.Multimap               as SMM

instance Hashable ByType

type ByTypeState = SMM.Multimap AbsType Client -- Multiple clients per hub

newByTypeRouter = do
  state <- SMM.newIO
  return $ Router (absType (Proxy::Proxy ByType)) (report_ state) (handler_ state)

    where
      report_ state = report "ByType Router" [
        ("Open Connections"
        ,bulletList . map (p . show) <$> (allConns state)
        )]

      allConns = atomically . T.toList . SMM.stream

      handler_ st bs client = do
        let hub@(ByType hubName) = decodeOK bs
        let conn = clientConn client
        liftIO $ dbg ["Protocol HUB",show hub,show client]
        atomically $ SMM.insert client hubName st
        loop (atomically $ SMM.delete client hubName st) $ do
          msg <- receiveMsg conn
          dbg [show hub,"IN",show . L.unpack $ msg]
          -- send to all but the message sender
          cs <- filter (/= client) <$> (atomically $ T.toList $ SMM.streamByKey hubName st)
          dbg [show hub,"TO",show cs]
          mapM_ (\c -> sendMsg (clientConn c) msg) cs
