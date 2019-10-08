{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Router.Echo
  ( newRouter
  ) where

import qualified Data.ByteString.Lazy as L
import qualified ListT                as T
import           Network.Router.Types
import           Network.Router.Util
import qualified StmContainers.Set    as S

-- PROB: debug option can be used for a denied service attack
newRouter = do
  state <- S.newIO
  return $
    router
      (Proxy :: Proxy (Echo ()))
      (handler_ state)
      (report_ state)
      (reportTyped state)
  where
    reportTyped state = do
      r <- typedBLOB . map asClientReport <$> allConns state
      return $ NestedReport "Echo" r []
    report_ state =
      report
        "Echo Router"
        [("Open Connections", bulletList . map (p . show) <$> allConns state)]
    allConns = atomically . T.toList . S.listT
    handler_ state _ echoBytes = do
      dbg ["Protocol ECHO started", show echoBytes]
      let echo :: Echo () = decodeOK echoBytes
      dbg ["Protocol ECHO", show echo]
      return $ Right (handle state echo)
    handle state echo client = do
      atomically $ S.insert client state
      loop (atomically $ S.delete client state) $ do
        msg <- fromClient client
        when (echoDebug echo) $ dbg ["ECHO", show $ L.unpack msg]
        dbg ["ECHO msg length=", show $ L.length msg]
        toClient client msg
