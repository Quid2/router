module Network.Router.Util(
  decodeOK,loop
  ,module X
  ) where

import           Control.Exception      (SomeException, fromException, handle)
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy   as L
import           Data.Flat
import           Network.Quid2
import           Network.Quid2    as X
import qualified Network.WebSockets     as WS

decodeOK bs = let Right v = unflat (L.pack bs) in v

loop deleteOp = handle (catchDisconnect deleteOp) . forever

catchDisconnect deleteOp ue = do
   deleteOp

   case fromException ue of
     Just e@(WS.CloseRequest _ _) -> dbg ["CloseRequest detected",show e]
     Just e -> warn ["Unexpected Connection exception detected",show e]
     _ -> err ["Unknown Exception detected",show ue]
