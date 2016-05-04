{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
module Main where

-- |Repository of absolute data types
-- with an embedded web interface to browse data types and access the Haskell/JS ... equivalents
import           Data.Bifunctor
import           Data.Foldable                        (toList)
import           Data.List                            (nub, sort, sortBy)
import qualified Data.Map                             as M
import           Data.Maybe
import           Data.Ord
import           Data.String
import           Network.Quid2                        hiding (solve, (<>))
import qualified Network.Wai
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Quid2.Util.Service
import           Repo.DB
import           Repo.Types
import           Text.Blaze.Html.Renderer.Text
import           Text.Blaze.Html5                     hiding (head, html, input,
                                                       main, map, output, param)
import           Text.Blaze.Html5.Attributes          hiding (async)
import           Web.Scotty

u = recordType def (Proxy::Proxy Repo)

main = initService "quid2-repo" setup

setup :: Quid2.Util.Service.Config () -> IO ()
setup cfg = do
  updateGlobalLogger rootLoggerName $ setLevel DEBUG -- INFO

  db <- openDB (stateDir cfg)

  async $ runClientForever def ByType $ \conn -> runEffect $ pipeIn conn >-> agent db >-> pipeOut conn

  sapp :: Network.Wai.Application <- scottyApp $ do
     middleware logStdoutDev
     get "/" $ do
       lst <- liftIO $ do
         DBState db <- wholeDB db
         return . renderHtml . dbIndex $ db
       html lst

     get "/type/:typeCode" $ do
        key <- unPrettyRef <$> param "typeCode"
        out <- liftIO $ do
          DBState env <- wholeDB db
          madt <- getDB db key
          return $ maybe "Unknown type" (renderHtml . pre . fromString . ppr . (env,)) madt
        html out

  let warpOpts = Warp.setPort 8000 . Warp.setTimeout 60 $ Warp.defaultSettings
  Warp.runSettings warpOpts sapp

    where

      dbIndex db = do
            table . mconcat . (tr (mconcat [th "Types",th "Unique Code"]) :) . map (\(adtS,r) -> tr $ mappend (td . toHtml $ a ! href (fromString $ "/type/"++ ppr r) $ toHtml adtS) (td . toHtml . ppr $ r)) . sortBy (comparing fst) . map (\(r,adt) -> (declName adt,r)) . M.toList $ db
            -- table . mconcat . (tr (mconcat [th "Types",th "Unique Code"]) :) . map (\(adtS,r) -> tr $ mappend (td . toHtml $ a ! href (fromString $ "/type/"++ ppr r) $ toHtml adtS) (td . toHtml . ppr $ r)) . sortBy (comparing fst) . map (\(r,adt) -> (adtName adt,r)) . M.toList $ db
            p $ mconcat [b "NOTE:"," The way the Types' unique codes are calculated will change and should not be relied upon."]
            -- table . mconcat . (tr (mconcat [th "Type(s)"]) :) . map (\(adtS,r) -> tr (td . toHtml $ a ! href (fromString $ "/type/"++ ppr r) $ toHtml adtS)) . sortBy (comparing fst) . map (\(r,adt) -> (adtName adt,r)) . M.toList $ db
         -- table . mconcat . (tr (mconcat [th "Types"]) :) . map (\(adtS,r) -> tr (td . toHtml $ a ! href (fromString $ "/type/"++ ppr r) $ toHtml adtS)) . sortBy (comparing fst) . map (\(r,adt) -> (adtName adt,r)) . M.toList $ db

      adtName = unwords . sort . map declName . toList

      agent db = do
        msg <- await
        case msg of
          Record adt -> lift $ putDB db (absRef adt) adt

          AskDataTypes -> do
            vs <- lift $ (\(DBState env) -> M.assocs env) <$> wholeDB db
            yield . KnownDataTypes $ vs

          Solve typ -> do
            rs <- lift $ mapM (\r -> (r,) <$> getDB db r) . nub . toList $ typ
            let errs = map fst . filter (isNothing . snd) $ rs
            yield . Solved typ $ if null errs
                                 then Right (map (second fromJust) rs)
                                 else Left $ unwords ["Unknown types:",show errs]
          _ -> return ()

        agent db

-- pp = head . toList . snd . head . M.elems . snd $ absTypeEnv (Proxy :: Proxy (Bool))
ppr = render . pPrint
