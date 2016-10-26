{-# LANGUAGE ScopedTypeVariables #-}
module Network.Router.Echo where

import Network.Router.Util
import Network.Router.Types

import qualified Data.ByteString.Lazy   as L
import qualified ListT                                as T
import qualified STMContainers.Set                    as S

-- PROB: debug option can be used for a denied service attack
newEchoRouter = do
   state <- S.newIO
   return $ router (Proxy::Proxy (Echo ())) (handler_ state) (report_ state) (reportTyped state)

     where
       reportTyped state = do
         r <- typedBLOB . map asClientReport <$> allConns state
         return $ NestedReport "Echo" r []

       report_ state = report "Echo Router" [("Open Connections",bulletList . map (p . show) <$> allConns state)]

       allConns = atomically . T.toList . S.stream

       handler_ state _ echoBytes client = do
         dbg ["Protocol ECHO started",show echoBytes]
         let echo :: Echo () = decodeOK echoBytes
         dbg ["Protocol ECHO",show echo]
         atomically $ S.insert client state
         loop (atomically $ S.delete client state) $ do
           msg <- fromClient client
           when (echoDebug echo) $ dbg ["ECHO",show $ L.unpack msg]
           dbg ["ECHO msg length=",show $ L.length msg]
           toClient client msg

