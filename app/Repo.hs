{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- |Repository of absolute data types
module Main where

import           Data.Bifunctor
import           Data.Foldable      (toList)
import           Data.List          (nub)
import           Data.Maybe
import           Data.Typed
import           Data.Word
import           Network.Router.API
import           Quid2.Util.Service
import           Repo.DB
import           Repo.Types
import           System.Timeout

t = do
  -- record (Proxy::Proxy Repo)
  -- solve (Proxy::Proxy Repo)
  record (Proxy::Proxy Int)
  solve (Proxy::Proxy Int)


main = initService "quid2-repo" setup

setup :: Quid2.Util.Service.Config () -> IO ()
setup cfg = do
  updateGlobalLogger rootLoggerName $ setLevel DEBUG -- INFO

  db <- openDB (stateDir cfg)

  runClientForever def (byType (Proxy::Proxy Repo)) $ \conn -> runEffect $ pipeIn conn >-> agent db >-> pipeOut conn

    where
      agent db = do
        msg <- await
        case msg of
          Record adt -> lift $ putDB db (absRef adt) adt
          Solve typ -> do
            rs <- lift $ mapM (\r -> (r,) <$> getDB db r) . nub . toList $ typ
            let errs = map fst . filter (isNothing . snd) $ rs
            yield . Solved typ $ if null errs
                                 then Right (map (second fromJust) rs)
                                 else Left $ unwords ["Unknown types:",show errs]
        agent db

record :: Typed a => Proxy a -> IO ()
record proxy = runClient def (byType (Proxy::Proxy Repo)) $
   \conn -> mapM_ (send conn . Record) . absADTs $ proxy

-- record :: Typed a => Proxy a -> IO ()

solve proxy = runClient def (byType (Proxy::Proxy Repo)) $ \conn -> do

  let typ = absType proxy
  send conn (Solve typ)

  let loop = do
        msg <- receive conn
        case msg of
          Solved t r | t == typ -> return r
          _ -> loop

  fromMaybe (Left "Timeout") <$> timeout (seconds 30) loop



