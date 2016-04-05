{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Network.Router.Types(
  Routers,Router(..),router
  ,Client,newClient,fromClient,toClient
  ,Word8
  ,module X) where

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

type Routers = M.Map AbsType Router

data Router = Router {
  routerKey    :: AbsType
  ,routerReport :: Report
  ,routerHandler :: Handler
  }

type Handler = AbsType -> [Word8] -> Client -> IO ()

router
  :: Model a =>
     Proxy a
     -> Report -> Handler -> Router
router proxy = let (TypeApp r _) = absType proxy
               in Router r

newClient n conn = do
  t <- getCurrentTime
  return $ Client n conn t

data Client = Client {
   clientId::Integer -- A unique value for the current server run
   ,clientConn::WS.Connection
   ,clientOpenTime::UTCTime
  }

fromClient = receiveMsg . clientConn
toClient = sendMsg . clientConn

instance Show Client where show c = unwords ["Client",show $ clientId c,show $ clientOpenTime c]

instance Eq Client where (==) c1 c2 = clientId c1 == clientId c2

instance Hashable Client where hashWithSalt salt c = hashWithSalt salt $ clientId c
instance Hashable a => Hashable (NonEmptyList a)
instance Hashable AbsRef
instance Hashable a => Hashable (Type a)

