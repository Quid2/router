{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections            ,ViewPatterns ,ScopedTypeVariables #-}
module Data.Typed.ToHaskell where

import           Control.Monad
import qualified Control.Monad.Reader    as R
import           Control.Monad.Trans.RWS
import           Data.Bifunctor
import           Data.Digest.Shake128
import           Data.Either
import           Data.Foldable
import           Data.Functor.Identity
import           Data.List
import qualified Data.Map                as M
import           Data.Maybe
import           Data.Ord
import qualified Data.Pattern            as P
import           Data.Typed              hiding (mdlName)
import           Data.Typed.Transform
import           Data.Word
import           System.Directory
import           System.FilePath
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import qualified Data.ByteString as B
-- data Module = Module {mdlName::String,mdlImports::[String]} deriving Show

 -- instance Pretty Module where
 --   pPrint m = text "module" <+> text (mdlName m) <+> text "where"
 --      <+> vcat (map (\s -> text "import" <+> text s) (mdlImports m))

 -- return $ Module (concat [declName adt,"_",prettyShow ref]) []

 {-
 Message "titto" _ (Bell _)|] 

 -- compare in binary, we need the types of the wildcards
 -- 
 match binaryVal = 
   chkBits [MessageCode..] chkBits [.."titto"] (skip :: Proxy Int) 

 skip :: const () <$> get 

 -- compare in code, need to be able to print out values corresponing to Vals (so know their types and figure out if they are infinite values)
 match v = case v of
   Message "titto" _ (Bell _) -> True
-}

srcDir = "/Users/titto/workspace/quid2-qq/src"

writePattern = writePattern_ srcDir

writePattern_ dir p pat = do
  body <- pattern dir p pat
  let name = ('P':) . concatMap hex . B.unpack . shake128 6 . encodeUtf8 . T.pack $ body
  let header = unlines [concat ["module QQ.Pattern.",name," (match) where"]
                       ,"import Prelude()"
                       ,"import Quid2.QQ.Pattern"]
  let mdl = header ++ body
  putStrLn mdl
  write (concat [dir,"/QQ/Pattern/",name,".hs"]) mdl
  return $ "QQ.Pattern." ++ name

-- |Generate code for Pattern match
pattern :: Model a => FilePath -> Proxy a -> P.Pattern P.WildCard -> IO String
pattern dir p pat = do
  let (t,e) = absoluteType p
  print t
  -- Create all types, the pattern depends on
  mdls <- writeModules dir (t,e)

  -- Convert to a matcher
  let convert (P.Con n ps) t =
        let adt = solvedADT e t
        in case consIn n adt of
          Nothing -> [Left $ unwords ["Constructor '",n,"' not present in",show adt]]
          Just (bs,ts) -> Right (MatchBits bs) : concatMap (uncurry convert) (zip ps ts)
      convert (P.Var P.WildCard) t = [Right $ MatchType $ solveF mdls t]
      convert (P.Val bs) _ = [Right $ MatchBits bs]

  let r = convert pat t
  if null (lefts r)
     then return $ srcMatch . rights $ r -- optMatch . 
     else error (unlines $ lefts r)

-- |A pattern matcher
data Match r = MatchBits [Bool]    -- Match a flattened value
             | MatchType (Type r)  -- Match a wildcard of given type
             deriving (Functor,Foldable,Traversable)

-- |Optimise matcher
optMatch (MatchBits bs:MatchBits bs':t) = optMatch $ MatchBits (bs ++ bs'):t 
optMatch (x:xs) = x : optMatch xs
optMatch [] = []

-- |Convert matcher to source code
srcMatch :: [Match DT] -> String
srcMatch ms = unlines $ concat [
  map (("import qualified " ++) . show . dtImport) $ concatMap toList ms
  ,["match :: ByteString -> Bool"]
  ,["match = run [" ++ (concat . intersperse "," . filter (not.null) . concatMap src $ ms) ++ "]"]
  ]
  where
    src (MatchBits [])                  = [[]]
    src (MatchBits bs) | length bs <= 8   = ss ["match8",show $ length bs,show $ asNum bs]
    src (MatchBits bs) | length bs <= 16  = ss ["match16",show $ length bs,show $ asNum bs]
    src (MatchBits bs) | length bs <= 32  = ss ["match32",show $ length bs,show $ asNum bs]
    src (MatchBits bs) | length bs <= 64  = ss ["match64",show $ length bs,show $ asNum bs]
    src (MatchBits bs) = let (bs',bs'') = splitAt 64 bs
                         in src (MatchBits bs') ++ src (MatchBits bs'')
    src (MatchType t) = ss ["skip (decode :: Get (",prettyShow (dtRef <$> t),"))"]
    ss = (:[]) . unwords
    --as8 bs = foldl (\v b -> b) (reverse bs) 0  -- sum . map (\(p,b) -> if b then p else 0) . zip (map (2^) [0..n]) $ bs
    asNum bs = sum . map (\(n,b) -> if b then 2^n else 0) . zip [0..] . reverse $ bs

-- |Monad for generating modules
type M = RWST MdlEnv () MdlState Identity

type MdlEnv = M.Map AbsRef AbsADT

-- |The generated modules
type MdlState = M.Map AbsRef DT

-- |A module
data Mdl = Mdl {mdlDir::String
               ,mdlName::String
               ,mdlSrc::String
               } deriving (Eq,Show)

mdlFileName r = concat [mdlDir r,"/",mdlName r,".hs"]

mdlImport r = text (mdlDir r) <> char '.' <> text (mdlName r)

-- |A data type, defined in a module.
data DT = DT {dtMdl::Mdl
             ,dtName::String
             } deriving (Show,Eq)

-- |reference to a module, e.g. QQ.Bool_H2637s7sd8
dtImport = mdlImport . dtMdl

-- |reference to a data type, e.g. QQ.Bool_H2637s7sd8.Bool
dtRef dt = dtImport dt <> char '.' <> text (dtName dt)

writeModules :: FilePath -> (AbsType,AbsEnv) -> IO MdlState
writeModules dir e =
  let st = modules e
  in do
    mapM_ out . nub . map dtMdl . M.elems $ st
    return st
       where
      out mdl = write (dir </> mdlFileName mdl) (mdlSrc mdl)

modules :: (AbsType,AbsEnv) -> MdlState
modules (t,e) = let ne = envRef2ADT . absEnv' $ e
                 in fst $ execRWS (mapM moduleE $ M.keys ne) ne (M.empty::MdlState)

moduleE :: AbsRef -> M DT
moduleE ref = do
  mMdl <- gets (M.lookup ref)
  maybe (moduleS ref) return mMdl

-- |Generate a module for a group of mutually recursive data types
moduleS :: AbsRef -> M DT
moduleS ref = do
      adt <- asks (fromJust . M.lookup ref) -- unused?
      -- let adt = adt0 {declName = LocalName $ declName adt0}
      adt' <- nonEmptyList . map (\adt -> adt {declName = LocalName $ declName adt}) . toList <$> traverse (traverse conv) adt
      name <- moduleNameD adt
      let src = vcat [text "{-# LANGUAGE DeriveGeneric ,DeriveDataTypeable ,EmptyDataDecls #-}"
                     ,text "module" <+> dtImport dt <+> text "where"
                     ,text "import Prelude()"
                     ,text "import Quid2.QQ.DataType"
                     ,vcat . map (\r -> text "import qualified" <+> dtImport r) $ extRefs adt'
                     ,vcat . map (\dt -> pPrint dt <+> text "deriving (Eq,Ord,Show,Typeable,Generic)") . toList $ adt'
                     ,vcat . map (text . instanceADT "Flat") . toList $ adt'
                     ]
          dt = DT (Mdl "QQ" (show name) (show src)) (adtName adt)
      modify (M.insert ref dt)
      return dt
    where
    conv (Ext r) = ExtP <$> moduleE r
    conv (Var v) = return $ VarP v
    conv (Rec s)  = return $ RecP s

    extRefs = nub . mapMaybe fromExt . concatMap toList . toList

    fromExt (ExtP d) = Just d
    fromExt _ = Nothing

    moduleNameD adt = hcat . punctuate (text "_") . map moduleNameD_ <$> (mapM refByName . sort . map declName . toList $ adt)
    -- BAD
    refByName n = asks ((n,) . fst . fromJust . find ((n==) . adtName . snd) . M.toList)
    moduleNameD_ (name,ref) = text name <> char '_' <> pPrint ref

data RefP = VarP Word8
          | RecP String
          | ExtP DT
          deriving (Show)

instance Pretty RefP where
  pPrint (VarP v) = varP v
  pPrint (RecP s) = text s
  pPrint (ExtP dt) = mdlImport (dtMdl dt) <> char '.' <> text (dtName dt)

instanceADT cls adt = instanceCtx cls (let LocalName n = declName adt in n) (fromIntegral . declNumParameters $ adt)

instanceCtx cls dt 0 = unwords ["instance",cls,dt]
instanceCtx cls dt n =
   let vs = map (:[]) $ take n ['a'..]
   in unwords ["instance (",intercalate "," . map (\v -> unwords [cls,v]) $ vs,") =>",cls,"(",dt,unwords vs,")"]

instanceS :: (AbsType,AbsEnv) -> String
instanceS (t,e) = R.runReader (instanceE t) e

instanceE (typeN -> TypeN t []) = do
  name <- R.asks (name t)
  return $ unwords ["instance AbsoluteType",name,"where absoluteType _ =","TypeCon (",show t,")"]

instanceE (typeN -> TypeN t ps) = do
  name <- R.asks (name t)
  let
      n = length ps
      vs = map (:[]) $ take n ['a'..]
      ctx = intercalate "," . map ("AbsoluteType " ++) $ vs
      body = strApp ("TypeCon " ++ par (show t)) $ map (\v -> concat ["absoluteType (Proxy::Proxy ",v,")"]) vs
   in return $ unwords ["instance (",ctx,") => AbsoluteType (",name,unwords vs,") where absoluteType _ =",body]

strApp = foldl (\h f -> unwords ["TypeApp",par f,par h])

par s = "(" ++ s ++ ")"

-- Env ops
name :: AbsRef -> AbsEnv -> String
name t = snd . solver t

solver :: AbsRef -> AbsEnv -> (AbsRef,String)
solver t = second adtName . refByExt t

refByExt :: AbsRef -> AbsEnv -> (AbsRef,AbsADT)
refByExt r = fromJust . find ((r==) . fst) . M.elems

adtName = declName . relADT

write f content = do
  createDirectoryIfMissing True (takeDirectory f)
  exists <- doesFileExist f
  -- unless exists $
  writeFile f content


