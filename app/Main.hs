-- -*- mode:org;mode:haskell; -*-
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        ,ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances ,BangPatterns #-}
module Main where

import           Control.Concurrent             (MVar, forkIO, modifyMVar,
                                                 modifyMVar_, newMVar, readMVar)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bifunctor
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as L
import           Data.Either
import           Data.List
import qualified Data.Map.Strict                as M
import           Data.Maybe
import qualified Data.Text                      as T
import qualified Data.Text.Lazy                 as TL
import           Data.Time.Util
import           Data.Word
import qualified Network.Router.ByAny
import qualified Network.Router.ByPattern
import qualified Network.Router.ByType
import qualified Network.Router.Echo
import           Network.Router.Types
import           Network.Top                    hiding (Config, first)
import qualified Network.Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets             as WS
import           Quid2.Util.Service
import           Repo.Disk                      (dbRepo)
import           System.IO                      (stdout)
import           Web.Scotty
import           ZM                             hiding (first)
import Network.Top.Util

{-
SOL: start manually as (already done by propellor):
killall -s SIGKILL quid2-net; /root/.cabal/bin/quid2-net > /dev/null 2>&1 &
-}
 {-
 Bug: quid2.net start won't work
 BUG: gets stuck after switching to websockets

 Switch pipe.

 Hosts all endpoints that require a fixed url address.

 http:80
 /  Return activity report.
?? /hook    HTTP call backs.

 https:443
 /ws       Accept WebSocket clients.

Max number of connections limited by ulimit -n , 1024 on Linux 256 on OSx

Vulnerabilities:
- no protection against denial of service attacks?
  --- huge messages
-}

serviceName = "quid2.net"

main = initService serviceName setup

-- data Port = Port {port::Int} deriving (Show,Read)

setup :: Config Int -> IO ()
setup cfg = do
  logLevelOut DEBUG stdout
  -- updateGlobalLogger rootLoggerName $ setLevel DEBUG -- INFO
  -- email titto serviceName "just started"

  let serverPort = fromMaybe 80 $ appConf cfg

  runServices

  -- local ADTS definition cache
  repo <- dbRepo (stateDir cfg)
  dbgS (stateDir cfg)

  --let adtSolver t = ((\env -> AbsoluteType (M.fromList env) t) <$>) <$> solveType repo def t
  let adtSolver t = solveType repo def t

  -- Keep track of open/closed connections
  warpState <- newWarpState
  let onOpen sockAddr  = connOpened warpState >> dbg ["Connection Open",show sockAddr] >> return True
  let onClose sockAddr = connClosed warpState >> dbg ["Connection Close",show sockAddr]

  -- Setup routers
  echoRouter <- Network.Router.Echo.newRouter
  bus <- newBus
  byAnyRouter <- Network.Router.ByAny.newRouter bus
  byTypeRouter <- Network.Router.ByType.newRouter bus
  byPatternRouter <- Network.Router.ByPattern.newRouter bus adtSolver
  let routers = [echoRouter,byAnyRouter,byTypeRouter,byPatternRouter]
  let routersMap = foldr (\r -> M.insert (routerKey r) r) M.empty routers
  let serverVersion = unwords [__DATE__,__TIME__,"(compiler local time)"]
  startupTime <- getCurrentTime
  serverReport <- newServiceReport serviceName serverVersion startupTime (warpReport warpState : map routerReport routers)
  --asText serverReport >>= dbg1

  sapp :: Network.Wai.Application <- scottyApp $ do

     -- middleware logStdoutDev -- NOTE: output on stdout, not log file
     get "/" $ liftIO (TL.pack <$> asHTML serverReport) >>= html

     -- Return Server report in binainspectorry format
     get "/report" $ do
       r <- liftIO (L.fromStrict . flat <$> warpBinaryReport serverVersion startupTime warpState (mapM routerBinaryReport routers))
       setHeader "Access-Control-Allow-Origin" "*"
       setHeader "Content-Type" "application/octet-stream"
       raw r

  --let warpOpts = Warp.setLogger noRequestLogger . Warp.setOnClose onClose . Warp.setOnOpen onOpen . Warp.setPort 80 . Warp.setTimeout 60 $ Warp.defaultSettings
  let warpOpts =  Warp.setOnClose onClose . Warp.setOnOpen onOpen . Warp.setPort serverPort . Warp.setTimeout 60 $ Warp.defaultSettings

  connCounter <- newMVar 0
  Warp.runSettings warpOpts $ WaiWS.websocketsOr (WS.defaultConnectionOptions {WS.connectionOnPong=dbgS "Pong!"}) (application connCounter routersMap) sapp -- staticApp

-- Embedded static files
-- staticApp :: Network.Wai.Application
-- staticApp = Static.staticApp $ Static.embeddedSettings $(embedDir "static")

-- application :: MVar ServerState -> WS.ServerApp
application :: MVar Integer -> Routers -> WS.PendingConnection -> IO ()
application st routers pending = do
    let r = WS.pendingRequest pending
    dbg ["Pending websocket request",show r]

    conn <- WS.acceptRequestWith pending (WS.AcceptRequest $ Just chatsProtocol)
    let
        fail reasons = do
          let msg = unwords $ "Error while initialising connection:" : reasons
          --dbg ["fail",msg]
          sendValue $ Failure msg -- (TL.toStrict msg)
          --threadDelay (seconds 15)
          --dbg ["fail close",msg]
          WS.sendClose conn (TL.pack msg)
          err reasons

        sendValue :: WSChannelResult -> IO ()
        sendValue = WS.sendBinaryData conn . flat

    when (WS.requestPath r /= "/ws") $ fail ["WebSockets connection allowed only at /ws"]
    --when (not $ WS.requestSecure r)  $ fail ["WebSockets request must be secure"]

    when (isNothing (find (== chatsProtocol) (WS.getRequestSubprotocols r))) $ fail ["Client must support WS protocol",concat ["'",T.unpack chatsProtocolT,"'"]]

    eProt <- WS.receiveData conn :: IO L.ByteString
    dbg ["header",show $ L.unpack eProt]
    case (unflat eProt) of
     Left e -> fail ["Bad protocol type data",show e]
     Right (TypedBLOB protType@(getTypes -> (rType,vTypes)) protBytes) -> do
       let bs = unblob protBytes
       dbg ["got router type",show protType,show protBytes,show $ B.unpack bs]
       case M.lookup rType routers of
        Just router -> do
          -- sendValue Success
          -- n <- connNum s
          -- client <- newClient n conn
          -- WS.forkPingThread conn 20
          -- handlerOK <- try (routerHandler router adtSolver vTypes bs client)
          -- case handlerOK of
          --   Left (e::SomeException) -> fail [show e,show protType]
          --   Right () -> return ()

          -- Validate connection first (this is not working)
          ehandler <- routerClientHandler router vTypes (L.fromStrict bs)
          dbg ["handler for",show protType,"present",show $ isRight ehandler]
          case ehandler of
            Left e -> fail [e,show protType]
            Right handler -> do
              n <- connNum st
              client <- newClient n conn
              sendValue Success
              WS.forkPingThread conn 20
              handler client

              -- loop (h $ ClientClose client) $ do
              --   msg <- fromClient client
              --   --when (echoDebug echo) $ dbg ["ECHO",show $ L.unpack msg]
              --   --dbg ["ECHO msg length=",show $ L.length msg]
              --   h ($ Client )mfwd <- hMsg handler (fwd )
              --   case mfwd of
              --     Nothing -> return ()
              --     Just tmsg -> undefined


              -- loop (hClose handler) $ do
              --   msg <- fromClient client
              --   --when (echoDebug echo) $ dbg ["ECHO",show $ L.unpack msg]
              --   --dbg ["ECHO msg length=",show $ L.length msg]
              --   mfwd <- hMsg handler
              --   case mfwd of
              --     Nothing -> return ()
              --     Just tmsg -> handleMsg undefined



          --Redirect
          -- sendValue (RetryAt $ accessPoint def)
          -- WS.sendClose conn (T.pack "Retry")
          -- -- Failure
        --   sendValue (Failure "You are NOT welcome")
        --   WS.sendClose conn (T.pack "Sad!")
        Nothing -> fail ["Unsupported Protocol",show protType]

--getTypes gt = let TypeN rType vTypes = typeN gt in (rType,map (\(TypeN t []) -> t) vTypes)
getTypes gt = let TypeN rType vTypes = typeN gt in (rType,map typeA vTypes)

-- unique connection number (in current server run)
connNum c = modifyMVar c (\n -> return (n+1,n))

 -- Number of opened and closed connections
type WarpState = MVar (Word64,Word64)

newWarpState = newMVar (0,0)

connOpened warpState = modifyMVar_ warpState (return . first (+1))

connClosed warpState =  modifyMVar_ warpState (return . second (+1))

warpReport :: WarpState -> Report
warpReport warpState = report "Warp Server"
                       [("Currently open connections"  ,p . show . (\(o,c) -> o-c) <$> readMVar warpState)
                       ,("Closed Connections",p . show . snd <$> readMVar warpState)
                       ]

warpBinaryReport
  :: String
     -> UTCTime
     -> WarpState
     -> IO [NestedReport TypedBLOB] -> IO (NestedReport TypedBLOB)
warpBinaryReport version startupTime warpState subs = do
  (o,c) <- readMVar warpState
  NestedReport "Warp" (typedBLOB $ WarpReport version (toTime startupTime) o c) <$> subs

-- t = absType (Proxy::Proxy WarpReport)

runServices = return ()


