{-# LANGUAGE TypeSynonymInstances, FlexibleInstances ,DeriveGeneric #-}
module Network.Router.Types(
  Routers,Router(..),router
  ,Client,newClient,fromClient,toClient,asClientReport
  ,Word8
  ,module X) where
import Data.Flat
import Data.Typed
import Data.Word
import Pandoc.Report as X
import Control.Concurrent.STM as X
import qualified Data.Map.Strict                      as M
import           Data.Hashable as X
import           Control.Applicative
import qualified Network.WebSockets                   as WS
import GHC.Generics
import Data.Typeable
import Data.Time.Clock
import Network.Quid2.Util
import qualified Data.ByteString.Lazy as L
import Report as X
import Data.Time.Util

type Routers = M.Map AbsType Router

data Router = Router {
  routerKey    :: AbsType
  ,routerHandler :: Handler
  ,routerReport :: Report
  ,routerBinaryReport::IO (NestedReport TypedBytes)
  }

type Handler = AbsType -> [Word8] -> Client -> IO ()

router proxy = let (TypeApp r _) = absType proxy
               in Router r

newClient n conn = do
  t <- getCurrentTime
  return $ Client n conn t

asClientReport c = ClientReport (clientId c) (toTime . clientOpenTime $ c)

data Client = Client {
   clientId::Integer -- A unique value for the current server run
   ,clientConn::WS.Connection
   ,clientOpenTime::UTCTime
  }

fromClient :: Client -> IO L.ByteString
fromClient = WS.receiveData . clientConn

toClient :: Client -> L.ByteString -> IO ()
toClient = WS.sendBinaryData . clientConn

instance Show Client where show c = unwords ["Client",show $ clientId c,show $ clientOpenTime c]

instance Eq Client where (==) c1 c2 = clientId c1 == clientId c2

instance Hashable Client where hashWithSalt salt c = hashWithSalt salt $ clientId c
instance Hashable a => Hashable (NonEmptyList a)
instance Hashable AbsRef
instance Hashable a => Hashable (Type a)

