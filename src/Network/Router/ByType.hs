{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Router.ByType
  ( newRouter
  ) where

-- -- -- import qualified Data.ByteString.Lazy   as L
import qualified ListT                  as T
import           Network.Router.Types
import           Network.Router.Util
import qualified StmContainers.Multimap as SMM

type State = SMM.Multimap AbsType Client

newState :: IO State
newState = SMM.newIO

entries = atomically . T.toList . SMM.listT

entriesByType st t = atomically . T.toList $ SMM.listTByKey t st

newRouter msgBus = do
  state <- newState
  bus <-
    onTypedMsg
      msgBus
      "Type"
      (\msgType msgBody -> forward (const True) state msgType msgBody)
  return $
    router
      (Proxy :: Proxy (ByType ()))
      (handle state bus)
      (reportTxt state)
      (reportTyped state)
  where
    reportTyped state = do
      r <-
        typedBLOB . ByTypeReport . map (\(t, e) -> (t, asClientReport e)) <$>
        entries state
      return $ NestedReport "ByType" r []
    reportTxt = report__ "ByType" entries
    report__ name sel state =
      report
        (name ++ " Router")
        [("Open Connections:", bulletList . map (p . show) <$> sel state)]
    handle st bus [t] bs = do
      let ByType :: ByType () = decodeOK bs
      return . Right $ route t bus st
    route t bus st me = do
      liftIO $ dbg ["Protocol ByType", show t, show me]
      atomically $ SMM.insert me t st
        -- on failure remove conn from map and terminate thread
      loop (atomically $ SMM.delete me t st) $ do
        msg <- fromClient me
          --dbg ["IN",show t,show source,show . L.unpack $ msg]
        forward (\dest -> me /= dest) st t msg
        output bus (TypedMsg t msg)
    forward sel st t msg = do
      destinations <- filter (\dest -> sel dest) <$> entriesByType st t
        --dbg ["TO",show t,show destinations]
      mapM_ (\e -> toClient e msg) destinations
