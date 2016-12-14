{-# LANGUAGE DeriveFunctor ,DeriveFoldable ,DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
module Data.Pattern.Matcher(
  MapTypeMatcher,typeMatcherMap,BTree(..),PatternMatcher,Match(..)
  ,BitMask(..),bitSplit,asMSBits,andMask,boolsSplit,mapPM,envPattern
  ) where
import Data.Pattern.Types
import qualified Data.ListLike.String as L
import           Data.Maybe
import           Data.Bifunctor
import           Data.Bits
import           Data.Either
import           Data.Typed           hiding ( Name, Val, Var)
import Data.Word
import Data.Int
import qualified Data.ByteString      as B
import           Data.Typed.Transform
import qualified Data.Map                  as M
-- Convert a Pattern to a Matcher

{-
Alternative coding more? efficient to check for validity of values (not really)

5:
00
01
10
110
111
-}

data MatchError = NotEnoughData | TooMuchData deriving (Eq,Show)

-- |Low level pattern, for efficient matching
data BitMask =
  Bits8 -- ^Match the indicated number of most significant bits with indicated value
  Int   -- ^numBits
  Word8 -- ^value has msb numBits set to expected valued, other bits are set to 0

  | Bits16 Int Word16
  | Bits32 Int Word32
  | Bits64 Int Word64

  -- | BVal BTree -- Constructor Tree
  deriving (Eq,Show)

pattern2Match :: AbsTypeModel -> Pattern WildCard -> Either String [Match AbsRef [Bool]]
pattern2Match (TypeModel t e) pat = errs $ convert pat t
  where
    convert (PCon n ps) t =
        let adt = solvedADT e t
        --in case consIn (T.unpack n) adt of
        in case constructorInfo (L.fromString n) adt of
          Nothing -> [Left $ unwords ["Constructor '"++ n++"' not present in",prettyShow t]]
          Just (bs,ts) -> Right (MatchBits bs) : concatMap (uncurry convert) (zip ps ts)
    -- convert (Var WildCard) t = [Right $ MatchType $ solveF mdls t]
    convert (PVar WildCard) t = [Right $ MatchType t]
    convert (PVal bs) _ = [Right $ MatchBits bs]

    errs r = if null (lefts r)
             then Right . optMatch . rights $ r
             else Left (unlines $ lefts r)


  {- For alternative encoding
  NO: in any case we still need to distinguish among constructors to check what follows.
  | Skip Int -- numBits to skip, for num constructors = power of two
  | BCons
    Int -- numBits
    Int -- max value to skip numbits, if val is greater then skip numBits+1
  -}

-- 5 -> BFork (BFork BVal BVal) (BFork BVal (BFork BVal BVal))
-- data BTree = BFork BTree BTree | BCon [BTree] deriving (Show,Eq)

x = bitSplit [True,True,False,True] == [Bits8 4 208]
xx = bitSplit8 [True,True,False,True,False,False,False,True,True] == [Bits8 8 209,Bits8 1 128]

--type PatternMatcher = (TypeMatchers,[Match AbsRef [BitMask]])
type PatternMatcher = (MapTypeMatcher,[Match AbsRef [Bool]])

-- TODO: leave only needed types
envPattern :: AbsTypeModel -> Pattern WildCard -> Either String PatternMatcher
-- envPattern at pat = second ((bitSplit8 <$>) <$>) . (matchTree at,) <$> pattern2Match at pat
envPattern at pat = (typeMatcherMap at,) <$> pattern2Match at pat

mapPM :: ([Bool] -> b) -> PatternMatcher -> (MapTypeMatcher,[Match AbsRef b])
mapPM f = second ((f <$>) <$>)

bitSplit8 [] = []
bitSplit8 bs =
  let (bs',bs'') = splitAt 8 bs
  in Bits8 (length bs') (asLSBits 8 bs') : bitSplit8 bs''

bitSplit [] = []
bitSplit bs | length bs <= 8   = [Bits8 (length bs) (asLSBits 8 bs)]
bitSplit bs | length bs <= 16  = [Bits16 (length bs) (asLSBits 16 bs)]
bitSplit bs | length bs <= 32  = [Bits32 (length bs) (asLSBits 32 bs)]
bitSplit bs | length bs <= 64  = [Bits64 (length bs) (asLSBits 64 bs)]
bitSplit bs =
  let (bs',bs'') = splitAt 64 bs
  in Bits64 64 (asLSBits 64 bs') : bitSplit bs''

boolsSplit :: Int -> [a] -> [[a]]
boolsSplit maxN bs | length bs <= maxN = [bs]
                   | otherwise = let (h,t) = splitAt maxN bs in h : boolsSplit maxN t

-- LSB
asLSBits :: Num c => Int -> [Bool] -> c
asLSBits n bs | n >= length bs = asNum bs

-- MSB
asMSBits :: Num c => Int -> [Bool] -> c
asMSBits n bs | n >= length bs = asNum (bs ++ replicate (n - length bs) False)

littleEndian16 w = (w .&. 255) * 256 + shiftR w 8

-- Little endian
-- asMSBitsLE n bs | n >= length bs && n `mod` 8 == 0 = asNum (bs ++ replicate (n - length bs) False)

y = map (andMask 8) [0..8]

m = asMSBits 16 [True,False,True]

-- n=1  -> 100000..
-- n=2  -> 11000..
andMask :: Num c => Int -> Int -> c
andMask tot n | tot>=n = asNum (replicate n True ++ replicate (tot-n) False)
              | otherwise = error $ unwords ["andMask",show tot, show n]
-- asNum bs = sum . map (\(n,b) -> if b then 2^n else 0) . zip [0..] . reverse $ bs
asNum bs = sum . map (\(n,b) -> if b then 2^n else 0) . zip [0..] . reverse $ bs

-- |A pattern matcher
data Match r bs = MatchBits bs    -- Match a flattened value
                | MatchType (Type r)  -- Match a wildcard of given type
  deriving (Show,Eq,Functor,Foldable,Traversable)

-- matchTypes :: [Match t] -> [Type t]
matchTypes = catMaybes . map matchType

matchType (MatchType t) = Just t
matchType _ = Nothing

-- |Optimise matcher
-- optMatch :: [Match t [Bool]] -> [Match t bs]
optMatch (MatchBits bs:MatchBits bs':t) = optMatch $ MatchBits (bs ++ bs'):t
optMatch (x:xs) = x : optMatch xs
optMatch [] = []

-- From Data.Typed.Dynamic

-- dd = matchTree (absTypeModel (Proxy::Proxy Word8)) --  [Bool])) -- [Bool])



-- |A matcher is an operation that will match a value of a given type without actually building a value
type MapTypeMatcher = M.Map AbsType BTree

typeMatcherMap :: AbsTypeModel -> MapTypeMatcher
typeMatcherMap tm =
  let
    env = typeTree tm
  --in map typeTree decEnv (typeName at)  (typeEnv at)
  in M.mapWithKey (\t _ -> simplifyBTree $ typeTr env t) env

data BTree = BFork BTree BTree
           | BCon [AbsType]
           | Skip Int
           deriving (Show,Eq)

simplifyBTree :: BTree -> BTree
simplifyBTree = rec sym

rec :: (BTree -> Maybe BTree) -> BTree -> BTree
rec f n@(BFork l r) =
      case f n of
        Nothing -> let n = BFork (rec f l) (rec f r)
                   in case f n of
                        Nothing -> n
                        Just nn -> nn
        Just nn -> nn

rec f n = case f n of
      Nothing -> n
      Just nn -> nn

sym (BFork (BCon []) (BCon [])) = Just $ Skip 1
sym (BFork (Skip n1) (Skip n2)) | n1 == n2 = Just $ Skip (n1+1)
sym _                           = Nothing

sl (BFork (BCon []) (BCon [])) = Just 1
sl (BFork l r)                 =( (,) <$> sl l <*> sl r) >>= snxt
snxt (n1,n2) | n1 == n2 = Just (n1+1)
snxt _       = Nothing

-- typeTr :: MapTypeTree -> AbsType -> BTree
typeTr = typeTree_ BFork (\t s bs ps -> BCon ps)

typeTree_ f c e t = conTree_ f c t [] (solve t e)

conTree_ f c t bs (ConTree l r) = f (conTree_ f c t (0:bs) l) (conTree_ f c t (1:bs) r)

-- conTree_ f c e t bs (Con cn cs) = c t (L.toString cn) (reverse bs) (map (typeTree_ f c e) (fieldsTypes cs))
conTree_ f c t bs (Con cn cs) = c t (L.toString cn) (reverse bs) (fieldsTypes cs)
