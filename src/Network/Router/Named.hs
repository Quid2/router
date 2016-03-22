{-# LANGUAGE ScopedTypeVariables #-}
module Network.Router.Named where

import Network.Hub.Util
import Network.Hub.Types

import qualified Data.ByteString.Lazy   as L
import qualified ListT                                as T
import qualified STMContainers.Set                    as S
import qualified STMContainers.Multimap               as SMM

-- A Producer/Consumer protocol
-- Messages from producers are sent to consumers
--data FanInOut a = Producer | Consumer

-- A named router, messages are sent to all other agents
-- Example: a public chat line
data Named = Named Name deriving (Eq, Ord, Typeable ,Show ,Read,Generic)
type Name = String

instance Binary NamedHub
instance HasModel NamedHub

type NamedHubState = SMM.Multimap Name Client -- Multiple clients per hub

newNamedHubRouter = do
  state <- SMM.newIO
  return $ Router (absType (Proxy::Proxy NamedHub)) (report_ state) (handler_ state)

    where
      report_ state = report "NamedHub Router" [("Open Conns",p . show <$> (atomically . T.toList . SMM.stream $ state))]

      -- allConns st = atomically . T.toList . SMM.stream $ st

      handler_ st bs client = do
        let hub@(NamedHub hubName) = dec bs
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



