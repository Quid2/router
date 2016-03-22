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
   return $ Router (absType (Proxy::Proxy Echo)) (report_ state) (handler_ state)

     where
       report_ state = report "Echo Router" [("Open Connections",p . show <$> (atomically . T.toList . S.stream $ state))]

       handler_ state echoBytes client = do
         dbg ["Protocol ECHO started",show echoBytes]
         let echo :: Echo = decodeOK echoBytes
         let conn = clientConn client
         dbg ["Protocol ECHO",show echo]
         atomically $ S.insert client state
         loop (atomically $ S.delete client state) $ do
           msg <- receiveMsg conn
           when (echoDebug echo) $ dbg ["ECHO",show $ L.unpack msg]
           dbg ["ECHO msg lenght=",show $ L.length msg] 
           sendMsg conn msg

