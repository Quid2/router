-- -*- mode:org;mode:haskell; -*-
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}
module Main where

import           Control.Applicative
import           Control.Concurrent                   (MVar, modifyMVar,
                                                       modifyMVar_, newMVar,
                                                       readMVar)
import           Control.Concurrent.STM
import           Control.Exception                    (fromException, handle)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bifunctor
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy                 as L
import           Data.FileEmbed                       (embedDir)
import           Data.Foldable                        (toList)
import           Data.List
import qualified Data.Map.Strict                      as M
import           Data.Maybe
import qualified Data.Text                            as T
import qualified Data.Text.Lazy                       as TL
import           Data.Time.Util
import           Data.Typed                           hiding (first)
import           Data.Word
import           Network.Router.ByType
import           Network.Router.Echo
import           Network.Router.Types
import           Network.Top                          hiding (Config, first)
import qualified Network.Top                          as Top
import qualified Network.Wai
import qualified Network.Wai.Application.Static       as Static
import qualified Network.Wai.Handler.Warp             as Warp
import qualified Network.Wai.Handler.WebSockets       as WaiWS
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Network.WebSockets                   as WS
import           Pandoc.Report
import           Quid2.Util.Service
import           Repo.DB
import qualified Repo.Types                           as R
import           System.Directory
import           System.IO                            (stdout)
import           Util
import           Web.Scotty

{-
SOL: start manually as (already done by propellor):
killall -s SIGKILL quid2-net; /root/.cabal/bin/quid2-net > /dev/null 2>&1 &
-}
 {-
 Bug: quid2.net start won't work
 BUG: gets stuck after switching to websockets

 Switch pipe.

 Hosts all endpoints that require a fixed url address.

 http:80nb
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

setup :: Config () -> IO ()
setup cfg = do
  logLevelOut DEBUG stdout
  -- updateGlobalLogger rootLoggerName $ setLevel DEBUG -- INFO
  -- email titto serviceName "just started"

  -- local ADTS definition cache
  db <- openDB (stateDir cfg)
  --dbgS (stateDir cfg)
  let dbRepo = R.Repo {R.get = \ref -> do
                          mr <- getDB db ref
                          --print (unwords ["get",show ref,show mr])
                          --dbg ["get",show ref,show $ isJust mr]
                          return mr
                      ,R.put = \adt -> do
                          --dbg ["put",prettyShow adt]
                          putDB db (refS adt) adt
                      }
  let adtSolver t = ((\env -> AbsoluteType (M.fromList env) t) <$>) <$> solveType dbRepo def t

  -- Keep track of open/closed connections
  warpState <- newWarpState
  let onOpen sockAddr  = connOpened warpState >> dbg ["Connection Open",show sockAddr] >> return True
  let onClose sockAddr = connClosed warpState >> dbg ["Connection Close",show sockAddr]

  -- Setup routers
  echoRouter <- newEchoRouter
  byTypeRouters <- newByTypeRouters
  let routers = [echoRouter]++byTypeRouters
  let routersMap = foldr (\r -> M.insert (routerKey r) r) M.empty routers
  let version = unwords [__DATE__,__TIME__,"(compiler local time)"]
  startupTime <- getCurrentTime
  serverReport <- newServiceReport serviceName version startupTime (warpReport warpState : map routerReport routers)
  --asText serverReport >>= dbg1

  sapp :: Network.Wai.Application <- scottyApp $ do
     -- middleware logStdoutDev -- NOTE: output on stdout, not log file
     get "/" $ liftIO (TL.pack <$> asHTML serverReport) >>= html
     get "/report" $ do
       r <- liftIO (flat <$> warpBinaryReport version startupTime warpState (mapM routerBinaryReport routers))
       setHeader "Access-Control-Allow-Origin" "*"
       setHeader "Content-Type" "application/octet-stream"
       raw r

  let warpOpts = Warp.setLogger noRequestLogger . Warp.setOnClose onClose . Warp.setOnOpen onOpen . Warp.setPort 80 . Warp.setTimeout 60 $ Warp.defaultSettings
  -- let warpOpts =  Warp.setOnClose onClose . Warp.setOnOpen onOpen . Warp.setPort 80 . Warp.setTimeout 60 $ Warp.defaultSettings

  connCounter <- newMVar 0
  Warp.runSettings warpOpts $ WaiWS.websocketsOr (WS.defaultConnectionOptions {WS.connectionOnPong=dbgS "Pong!"}) (application adtSolver connCounter routersMap) sapp -- staticApp

-- Embedded static files
-- staticApp :: Network.Wai.Application
-- staticApp = Static.staticApp $ Static.embeddedSettings $(embedDir "static")

-- application :: MVar ServerState -> WS.ServerApp
application :: R.TypeSolver -> MVar Integer -> Routers -> WS.PendingConnection -> IO ()
application adtSolver st routers pending = do
    let r = WS.pendingRequest pending
    dbg ["Pending websocket request",show r]

    conn <- WS.acceptRequestWith pending (WS.AcceptRequest $ Just chatsProtocol)
    let
        fail reasons = do
          let msg = TL.pack . unwords $ "Error while initialising connection:" : reasons
          sendValue $ Failure (TL.toStrict msg)
          WS.sendClose conn msg
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
       dbg ["got router type",show protType,show protBytes,show $ L.unpack bs]
       case M.lookup rType routers of
        Just router -> do
          -- sendValue Success
          -- n <- connNum st
          -- client <- newClient n conn
          -- WS.forkPingThread conn 20
          -- handlerOK <- try (routerHandler router adtSolver vTypes bs client)
          -- case handlerOK of
          --   Left (e::SomeException) -> fail [show e,show protType]
          --   Right () -> return ()

          ehandler <- routerHandler router adtSolver vTypes bs
          case ehandler of
            Left e -> fail [e,show protType]
            Right handler -> do
              sendValue Success
              n <- connNum st
              client <- newClient n conn
              WS.forkPingThread conn 20
              handler client

          --Redirect
          -- sendValue (RetryAt $ accessPoint def)
          -- WS.sendClose conn (T.pack "Retry")
          -- -- Failure
        --   sendValue (Failure "You are NOT welcome")
        --   WS.sendClose conn (T.pack "Sad!")
        Nothing -> fail ["Unsupported Top Protocol",show protType]

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



