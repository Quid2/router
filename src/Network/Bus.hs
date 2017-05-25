module Network.Bus(Connection(..),newBus,joinBus) where
import Control.Concurrent.STM
-- -- import Control.Concurrent.STM.TChan
import Network.Top.Types(Connection(..))

t = do
  bus <- newBus
  c1 <- joinBus bus 'a'
  c2 <- joinBus bus 'b'
  output c1 True
  output c2 False
  False <- input c1
  True <- input c2
  print "OK"

type Bus source value = TChan (source,value)

newBus :: IO (TChan a)
newBus = atomically newBroadcastTChan

joinBus :: Eq a1 => TChan (a1, a) -> a1 -> IO (Connection a)
joinBus bus source = do
  chan <- atomically $ dupTChan bus
  return Connection {
    input = loopInput source chan
    ,output = \value -> atomically $ writeTChan chan (source,value)
    ,close = error "unimplemented"
  }

loopInput source chan = do
  (s,v) <- atomically $ readTChan chan
  if s==source
    then loopInput source chan
    else return v

