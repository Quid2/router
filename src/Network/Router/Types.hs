{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Network.Router.Types(
  Routers,Router(..),Client(..)
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

type Routers = M.Map AbsType Router

data Router = Router {
  routerKey    :: AbsType
  ,routerReport :: Report
  ,routerHandler :: [Word8] -> Client -> IO ()
  }

data Client = Client {
   clientId::Integer -- A unique value for the current server run
   ,clientConn::WS.Connection
  }

instance Show Client where show c = unwords ["Client",show $ clientId c]

instance Eq Client where (==) c1 c2 = clientId c1 == clientId c2

instance Hashable Client where hashWithSalt salt c = hashWithSalt salt $ clientId c
instance Hashable a => Hashable (NonEmptyList a)
instance Hashable AbsRef
instance Hashable a => Hashable (Type a)
