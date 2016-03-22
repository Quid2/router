{-# LANGUAGE ScopedTypeVariables #-}
module Network.Hub.Router.Pattern(
  runClient,newRouter,module X
  ) where

-- TODO: factor out duplicate code with RouterClass
import Network.Hub.Util
import Network.Hub.Types

import qualified Data.ByteString.Lazy   as L
import qualified ListT                                as T
import qualified STMContainers.Set                    as S
import qualified STMContainers.Multimap               as SMM
import Language.Quid2.Pattern as X

-- A Producer/Consumer protocol
-- Messages from producers are sent to consumers
--data FanInOut a = Producer | Consumer

-- TODO: allow the pattern to be changed dynamically:
-- Note: this creates an asymmetry between what goes up:
data Protocol v  = Value v
                 | Filter (Pattern WildCard)
-- And what comes down: v

-- A named router, messages are sent to all other agents
-- Example: a public chat line
data ClassFiltered = ClassFiltered Name (Pattern WildCard) deriving (Eq, Ord, Typeable ,Show, Generic)
type Name = AbsType

instance Binary ClassFiltered
instance HasModel ClassFiltered

instance Hashable ClassFiltered

type ClassFilteredState = SMM.Multimap Name Client -- Multiple clients per hub

newRouter = do
  state <- SMM.newIO
  return $ Router (absType (Proxy::Proxy ClassFiltered)) (report_ state) (handler_ state)

    where
      report_ state = report "ClassFiltered Router" [("Open Conns",p . show <$> (atomically . T.toList . SMM.stream $ state))]

      handler_ st bs client = do
        let hub@(ClassFiltered hubName pat) = dec bs
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

--runClient :: HasModel a => Proxy a -> PatQ ->  (Connection -> IO r) -> IO r
runClient proxy pat client = runWSClient $ \conn -> do
  protocol conn (ClassFiltered (absType proxy) pat)
  client conn

