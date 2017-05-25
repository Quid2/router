{-# LANGUAGE ScopedTypeVariables ,OverloadedStrings #-}
module ZM.GHC where

import qualified Bag                 as GHC
import           Control.Exception
import qualified Data.IntSet         as I
import           Data.IORef
import qualified DynFlags            as GHC
import qualified ErrUtils            as GHC
import qualified GHC
import qualified GHC.Paths           as GHC
import qualified HscTypes            as GHC
import qualified MonadUtils          as GHC
import           Network.Router.Util
import qualified Outputable          as GHC
import           System.FilePath
import           System.Log
import qualified Data.ByteString as B
import Unsafe.Coerce
import System.TimeIt
import System.Plugins.Load

--loadModule ::  FilePath -> [String] -> String -> IO (Either [String] Bool)
loadValue codeDir pkgs mdlName =
   doWithErrors $ do
    info ["loadValue",codeDir,show pkgs,mdlName]
    dflags <- GHC.getSessionDynFlags
    GHC.setSessionDynFlags $ dflags {
        GHC.ghcMode = GHC.CompManager -- GHC.OneShot?
        ,GHC.ghcLink = GHC.LinkInMemory
        ,GHC.hscTarget = GHC.HscInterpreted -- GHC.HscAsm --
         --,GHC.hscTarget = GHC.HscAsm -- ?
        --,GHC.optLevel = 1 -- CHK: should it be higher? would it be slower?
        ,GHC.importPaths = [codeDir]
        -- Can access everything? imports to non-quid2 modules should fail when checking for dependencies.
        ,GHC.packageFlags = map (\pkg -> GHC.ExposePackage (GHC.PackageArg pkg) $ GHC.ModRenaming True []) pkgs
        ,GHC.generalFlags = I.fromList [fromEnum GHC.Opt_HideAllPackages]
        -- ,GHC.settings = (GHC.settings dflags) {GHC.sTmpDir=tmpDir}
        -- ,GHC.extensionFlags = flags
        }
    target <- GHC.guessTarget mdlName Nothing
    liftIO $ print $ showS . GHC.ppr $ target
    GHC.setTargets [target]
    r <- fmap GHC.succeeded (GHC.load GHC.LoadAllTargets)
    when (not r) $ error $ concat ["Loading module",mdlName,"failed"]
    -- Bringing the module into the context
    --- GHC.setContext [GHC.IIDecl $ GHC.simpleImportDecl (GHC.mkModuleName mdlName)]
    GHC.setContext [GHC.IIModule $ GHC.mkModuleName mdlName]
    -- evaluating and running an action
    act :: B.ByteString -> Bool <- unsafeCoerce <$> GHC.compileExpr (concat [mdlName,".match"])

    info ["compileModule]"]
    return $ Just r

compileModule ::  FilePath -> [String] -> String -> IO (Either [String] ())
compileModule codeDir pkgs mdlName =
   doWithErrors $ do
    info ["compileModule",codeDir,show pkgs,mdlName]
    dflags <- GHC.getSessionDynFlags
    GHC.setSessionDynFlags $ dflags {
        GHC.ghcMode = GHC.CompManager -- GHC.OneShot?
        --,GHC.ghcLink = GHC.LinkInMemory
        --,GHC.hscTarget = GHC.HscInterpreted -- GHC.HscAsm --
         --,GHC.hscTarget = GHC.HscAsm -- ?
        --,GHC.optLevel = 1 -- CHK: should it be higher? would it be slower?
        ,GHC.importPaths = [codeDir]
        -- Can access everything? imports to non-quid2 modules should fail when checking for dependencies.
        ,GHC.packageFlags = map (\pkg -> GHC.ExposePackage (GHC.PackageArg pkg) $ GHC.ModRenaming True []) pkgs
        ,GHC.generalFlags = I.fromList [fromEnum GHC.Opt_HideAllPackages]
        -- ,GHC.settings = (GHC.settings dflags) {GHC.sTmpDir=tmpDir}
        -- ,GHC.extensionFlags = flags
        }
    target <- GHC.guessTarget mdlName Nothing
    liftIO $ print $ showS . GHC.ppr $ target
    GHC.setTargets [target]
    r <- fmap GHC.succeeded (GHC.load GHC.LoadAllTargets)
    when (not r) $ error $ concat ["Loading module",mdlName,"failed"]
    info ["compileModule]"]
    return $ Just ()

doWithErrors :: GHC.Ghc (Maybe a) -> IO (Either [String] a)
doWithErrors action = do
    codeErrors <- newIORef []
    --protectHandlers $
    catch (wrapper codeErrors) $ \ (e :: SomeException) -> do
        errs <- readIORef codeErrors
        return (Left errs)
  where
    wrapper codeErrors = fixupErrors codeErrors =<< do
        GHC.defaultErrorHandler (writeErr codeErrors) (GHC.FlushOut $ return ()) -- ??? CHECK
            $ GHC.runGhc (Just GHC.libdir)
            $ GHC.handleSourceError (handle codeErrors)
            $ do
                dflags <- GHC.getSessionDynFlags
                GHC.setSessionDynFlags dflags {
                    GHC.log_action = logAction codeErrors
                    }
                action

    logAction errs dflags severity span style msg = do
        let niceError = showS $ GHC.withPprStyle style $ GHC.mkLocMessage severity span msg
        writeErr errs niceError

    writeErr ref err = modifyIORef ref (++ [ err ]) -- print ("HANDLED" ++ show err) >>

    handle ref se = do
        let errs    = GHC.bagToList (GHC.srcErrorMessages se)
            cleaned = map (showS . GHC.errMsgShortDoc) errs -- GHC.errMsgExtraInfo
        GHC.liftIO $ modifyIORef ref (++ cleaned)
        return Nothing

    fixupErrors errs (Just x) = return (Right x)
    fixupErrors errs Nothing  = fmap Left (readIORef errs)

showS :: GHC.SDoc -> String
showS = GHC.showSDocUnsafe

