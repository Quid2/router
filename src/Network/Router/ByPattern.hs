{-# LANGUAGE ScopedTypeVariables #-}
module Network.Router.ByPattern where

import Network.Router.Util
import Network.Router.Types
import qualified Data.ByteString.Lazy   as L
import qualified ListT                                as T
import qualified STMContainers.Set                    as S
import qualified STMContainers.Multimap             as SMM
import Data.Pattern.Haskell(matcher,match)
import Data.Pattern.Types
instance Hashable ByPattern
instance Hashable v => Hashable (Pattern v)
instance Hashable Var
instance Hashable WildCard

m proxy pat = let Right pp = envPattern (absoluteType proxy) pat in matcher pp

type ByPatternState = SMM.Multimap AbsType Client -- Multiple clients per hub

newByPatternRouter = do
  state <- SMM.newIO
  return $ Router (absType (Proxy::Proxy ByPattern)) (report_ state) (handler_ state)

    where
      report_ state = report "ByPattern Router" [("Open Conns",p . show <$> (atomically . T.toList . SMM.stream $ state))]

      -- allConns st = atomically . T.toList . SMM.stream $ st

      handler_ st bs client = do
        let hub@(ByPattern hubName) = decodeOK bs
        let conn = clientConn client
        liftIO $ dbg ["Protocol HUB",show hub,show client]
        atomically $ SMM.insert client hubName st
        loop (atomically $ SMM.delete client hubName st) $ do
          msg <- receiveMsg conn
          dbg [show hub,"IN",show . L.unpack $ msg]
          -- send to all but the message sender
          cs <- filter (/= client) <$> (atomically $ T.toList $ SMM.streamByKey hubName st)
          dbg [show hub,"TO",show cs]
          mapM_ (\c -> sendMsg (clientConn c) msg) cs

