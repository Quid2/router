{-# LANGUAGE OverloadedStrings ,StandaloneDeriving  #-}
module Pandoc.Report(Report(..),report,newServiceReport,asText,asHTML,p,bulletList) where

import           Control.Applicative
import           Data.Default
import           Text.Pandoc.Builder
import           Text.Pandoc.Writers.HTML
import           Text.Pandoc.Writers.Markdown
-- import Data.Monoid
import System.Posix.Process(getProcessID,getProcessTimes,ProcessTimes(..))
-- import System.Linux.Proc(getProcessStatus)
import Data.Time.Clock
import Data.Time.Format


data Report = Report {reportTitle::Inlines
                     ,reporters::[(String,Reporter)]
                     ,subreports::[Report]}

type Reporter = IO Blocks

report :: String -> [(String, Reporter)] -> Report
report title rs = Report (str title) rs []

newServiceReport
  :: String
     -> String
     -> String
     -> [Report] --  [(String,Reporter)]
     -> IO Report
newServiceReport serviceName versionMajor versionMinor rs =  do
    t <- timeDateTime

    let title = "Service " <> str serviceName

    --return $ Report (setTitle title . doc) $ [
    return $ Report title [
       ("Version",return . p $ versionMajor <> " " <> versionMinor)
      ,("Started up at",return . p $  t)
      ,("Report generated at",p <$> timeDateTime)
      ,("Process Times",p. show <$> getProcessTimes)
      ] rs

p = plain . str

asPandoc :: Report -> IO Pandoc
asPandoc r = doc <$> asBlocks r

asBlocks :: Report -> IO Blocks
asBlocks r = do
  bs <- bulletList <$> (mapM (\(n,c) -> ( (p $ n <> " ") <>) <$> c) $ reporters r)
  rs <- bulletList <$> (mapM asBlocks $ subreports r) 
  return $ header 2 (reportTitle r) <> bs <> rs

asText :: Report -> IO String
asText r = writePlain def <$> asPandoc r

asHTML :: Report -> IO String
asHTML r = writeHtmlString def <$> asPandoc r

timeDateTime = timeF "%F %H:%M.%S"
timeF format = fmap (formatTime defaultTimeLocale format) getCurrentTime

deriving instance Show ProcessTimes

-- getCurrentProcessStatus = getProcessID >>= getProcessStatus . fromInteger . toInteger
-- getCurrentProcessInfo = getProcessID >>= procGetStatInfo . fromInteger . toInteger


{-
    -- return . scottyHTML $
    let head = H.docTypeHtml $ do
          let h = fromString $ unwords ["Service",serviceName]
          H.head $ do
            H.title h
          H.body $ do
            H.h1 h
            H.p $ fromString $ unwords ["Started up at",t]
    return (head,[])
-}

{-
asHTML (h,_) = scottyHTML $ h

scottyHTML = decodeUtf8 . renderHtml

  reportMem <- STM.newTVarIO Nothing

  (userSeal,userOut) <- reportMBox (pr reportMem)

    get "/" $ html serverReport

    get "/report" $ do
      report <- liftIO $ atomically $ STM.readTVar reportMem
      html . scottyHTML . reportAsHTML $ report

      -- Send output by email and also save it for display on /report
      pr reportMem msg = do
        t <- timeDateTime
        let tmsg = (t,msg)
        print tmsg
        email titto "stocks" msg
        -- say m
        atomically $ STM.modifyTVar' reportMem (Just . maybe [tmsg] (tmsg:))


---------- Server Report

reportAsHTML :: Maybe Report -> H.Html
reportAsHTML mr = H.docTypeHtml $ do
    H.head $ do
        H.title "Events"
    H.body $ do
        H.h1 "Events"
        maybe
          (H.p "No events.")
          (H.ul . mapM_ (\(d,m) -> H.li . H.toHtml $ (unwords [d,":",m])))
          mr

reportMBox :: (b -> IO ()) -> IO (STM (), Output b)
reportMBox pr = do
  (output, input, seal) <- spawn' Unbounded
  mbox <- async $ do runEffect $ fromInput input >-> for cat (liftIO . pr) -- >>
  return (seal,output) --  (mbox,output)
-}
