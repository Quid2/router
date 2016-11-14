{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Network.Router.ByAny(newRouter) where

import qualified ListT                as T
import           Network.Router.Types
import           Network.Router.Util
import qualified STMContainers.Set    as S

type State = S.Set Client

newState = S.newIO
entries = atomically . T.toList . S.stream

newRouter msgBus = do
  state <- newState
  bus <- onTypedMsg msgBus "Any" (\ msgType msgBody -> entries state >>= mapM_ (\e -> toClient e $ flat $ TypedBLOB msgType (blob FlatEncoding msgBody)))

  return $ router (Proxy::Proxy (ByAny ())) (handle state bus) (reportTxt state) (reportTyped state)

    where

      reportTyped state = do
         r <- typedBLOB . ByAnyReport . map asClientReport <$> entries state
         return $ NestedReport "ByAny" r []

      reportTxt = report__ "ByAny" entries

      report__ name sel state =  report (name++" Router") [
        ("Open Connections:"
        ,bulletList . map (p . show) <$> sel state
        )]

      handle st bus [t] _ = do
        when (t /= typedBLOBType) $ error (unwords ["Unsupported type",show t])
        return $ route bus st

      route bus st me = do
        liftIO $ dbg ["Protocol ByAny",show me]

        atomically $ S.insert me st
        -- on failure remove conn from map and terminate thread
        loop (atomically $ S.delete me st) $ do
          msg <- fromClient me
          case unflat msg of
            Left _ -> dbg ["routeAny, bad format msg",show msg]
            Right (TypedBLOB t (unblob -> tmsg)) -> do
              -- forward st t tmsg
              entries st >>= mapM_ (\dest -> when (me /= dest) $ toClient dest msg)
              output bus (TypedMsg t tmsg)

typedBLOBType = absType (Proxy::Proxy TypedBLOB)
