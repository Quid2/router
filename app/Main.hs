{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

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
import           Network.Top.Util
import           Repo
 {-
 TOP router

 http:80
 /          Return HTML activity report.
 /report    Return binary activity report
 /ws       Accept WebSocket clients.

Start manually as:
killall -s SIGKILL top-router; /root/.cabal/bin/top-router > /dev/null 2>&1 &

Note: Max number of connections limited by ulimit -n , 1024 on Linux, 256 on OSx
-}

import qualified Network.Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets             as WS
import           Quid2.Util.Service
import           Repo.Auto                      (autoRepo)
import           Repo.Disk                      (dbRepo)
import qualified Repo.Types                     as R
import           System.IO                      (stdout)
import           Web.Scotty
import           ZM                             hiding (first)

serviceName = "top-router"

main = initService serviceName setup

data MyConfig =
  MyConfig
    { debugLevel :: Priority
    -- , wwwPort    :: Int has to be 80
    }
  deriving (Show, Read)

setup :: Config MyConfig -> IO ()
setup cfg = do
  let appCnf = appConf cfg
  let serverPort = 80 -- fromMaybe 80 (wwwPort <$> appCnf)
  let dbgLevel = maybe WARNING debugLevel appCnf
  logLevelOut dbgLevel stdout
  dbgS $ show cfg
  -- t = absType (Proxy::Proxy WarpReport)
  -- updateGlobalLogger rootLoggerName $ setLevel DEBUG -- INFO
  -- email titto serviceName "just started"
  -- runServices
  -- local ADTS definition cache
  --let adtSolver t = ((\env -> AbsoluteType (M.fromList env) t) <$>) <$> solveType repo def t
  -- CHECK: very slow? should this service be embedded with the router given that we depend on it for Pattern channel support?
  --setupRouters (run . getAbsTypeModel repo) >>= www serverPort
  -- Embedded ADT record service 
  repo <- dbRepo (stateDir cfg) >>= autoRepo
  setupRouters (getAbsTypeModel repo) >>= www serverPort

setupRouters :: (AbsType -> IO (Either String AbsTypeModel)) -> IO Routers
setupRouters adtSolver = do
  echoRouter <- Network.Router.Echo.newRouter
  bus <- newBus
  byAnyRouter <- Network.Router.ByAny.newRouter bus
  byTypeRouter <- Network.Router.ByType.newRouter bus
  byPatternRouter <- Network.Router.ByPattern.newRouter bus adtSolver
  let routers = [echoRouter, byAnyRouter, byTypeRouter, byPatternRouter]
  return $ foldr (\r -> M.insert (routerKey r) r) M.empty routers

www :: Int -> Routers -> IO ()
www serverPort routersMap = do
  startupTime <- getCurrentTime
  warpState <- newWarpState
  let onOpen sockAddr =
        connOpened warpState >> dbg ["Connection Open", show sockAddr] >>
        return True
  let onClose sockAddr =
        connClosed warpState >> dbg ["Connection Close", show sockAddr]
  let routers = M.elems routersMap
  serverReport <-
    newServiceReport
      serviceName
      serverVersion
      startupTime
      (warpReport warpState : map routerReport routers)
  binaryReport <-
    warpBinaryReport
      serverVersion
      startupTime
      warpState
      (mapM routerBinaryReport routers)
      --asText serverReport >>= dbg1
  --let warpOpts = Warp.setLogger noRequestLogger . Warp.setOnClose onClose . Warp.setOnOpen onOpen . Warp.setPort 80 . Warp.setTimeout 60 $ Warp.defaultSettings
  let warpOpts =
        Warp.setOnClose onClose .
        Warp.setOnOpen onOpen . Warp.setPort serverPort . Warp.setTimeout 60 $
        Warp.defaultSettings
  -- | Keep track of open/closed connections
  connCounter <- newMVar 0
  app <-
    scottyApp $
      -- middleware logStdoutDev -- NOTE: output on stdout, not log file
     do
      get "/" $ liftIO (TL.fromStrict <$> asHTML serverReport) >>= html
    -- Return Server report in binary format
      get "/report" $ do
        let r = L.fromStrict . flat $ binaryReport
        setHeader "Access-Control-Allow-Origin" "*"
        setHeader "Content-Type" "application/octet-stream"
        raw r
  Warp.runSettings warpOpts $
    WaiWS.websocketsOr
      (WS.defaultConnectionOptions {WS.connectionOnPong = dbgS "Pong!"})
      (application connCounter routersMap)
      app

serverVersion = unwords [__DATE__, __TIME__, "(compiler local time)"]

-- Embedded static files
-- staticApp :: Network.Wai.Application
-- staticApp = Static.staticApp $ Static.embeddedSettings $(embedDir "static")
-- application :: MVar ServerState -> WS.ServerApp
application :: MVar Integer -> Routers -> WS.PendingConnection -> IO ()
application st routers pending = do
  let r = WS.pendingRequest pending
  dbg ["Pending websocket request", show r]
  conn <-
    WS.acceptRequestWith pending (WS.AcceptRequest (Just chatsProtocol) [])
  let fail reasons = do
        let msg = unwords $ "Error while initialising connection:" : reasons
          --dbg ["fail",msg]
        sendValue $ Failure msg -- (TL.toStrict msg)
          --threadDelay (seconds 15)
          --dbg ["fail close",msg]
        WS.sendClose conn (TL.pack msg)
        err reasons
      sendValue :: WSChannelResult -> IO ()
      sendValue = WS.sendBinaryData conn . flat
  when (WS.requestPath r /= "/ws") $
    fail ["WebSockets connection allowed only at /ws"]
    --when (not $ WS.requestSecure r)  $ fail ["WebSockets request must be secure"]
  when (isNothing (find (== chatsProtocol) (WS.getRequestSubprotocols r))) $
    fail
      [ "Client must support WS protocol"
      , concat ["'", T.unpack chatsProtocolT, "'"]
      ]
  eProt <- WS.receiveData conn :: IO L.ByteString
  dbg ["header", show $ L.unpack eProt]
  case (unflat eProt) of
    Left e -> fail ["Bad protocol type data", show e]
    Right (TypedBLOB protType@(getTypes -> (rType, vTypes)) protBytes) -> do
      let bs = unblob protBytes
      dbg ["got router type", show protType, show protBytes, show $ B.unpack bs]
      case M.lookup rType routers of
        Just router
          -- sendValue Success
          -- n <- connNum s
          -- client <- newClient n conn
          -- WS.forkPingThread conn 20
          -- handlerOK <- try (routerHandler router adtSolver vTypes bs client)
          -- case handlerOK of
          --   Left (e::SomeException) -> fail [show e,show protType]
          --   Right () -> return ()
          -- Validate connection first (this is not working)
         -> do
          ehandler <- routerClientHandler router vTypes (L.fromStrict bs)
          dbg ["handler for", show protType, "present", show $ isRight ehandler]
          case ehandler of
            Left e -> fail [e, show protType]
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
        Nothing -> fail ["Unsupported Protocol", show protType]

--getTypes gt = let TypeN rType vTypes = typeN gt in (rType,map (\(TypeN t []) -> t) vTypes)
getTypes gt =
  let TypeN rType vTypes = typeN gt
   in (rType, map typeA vTypes)

-- unique connection number (in current server run)
connNum c = modifyMVar c (\n -> return (n + 1, n)) -- Number of opened and closed connections

type WarpState = MVar (Word64, Word64)

newWarpState = newMVar (0, 0)

connOpened warpState = modifyMVar_ warpState (return . first (+ 1))

connClosed warpState = modifyMVar_ warpState (return . second (+ 1))

warpReport :: WarpState -> Report
warpReport warpState =
  report
    "Warp Server"
    [ ( "Currently open connections"
      , p . show . (\(o, c) -> o - c) <$> readMVar warpState)
    , ("Closed Connections", p . show . snd <$> readMVar warpState)
    ]

warpBinaryReport ::
     String
  -> UTCTime
  -> WarpState
  -> IO [NestedReport TypedBLOB]
  -> IO (NestedReport TypedBLOB)
warpBinaryReport version startupTime warpState subs = do
  (o, c) <- readMVar warpState
  NestedReport "Warp" (typedBLOB $ WarpReport version (toTime startupTime) o c) <$>
    subs
