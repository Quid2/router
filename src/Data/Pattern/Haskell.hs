module Data.Pattern.Haskell where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
-- import qualified Data.ByteString.Internal as B
-- import qualified Data.Map                 as M
import           Control.Monad
import           Data.Binary.Bits.Get
import           Data.Bits
import           Data.Pattern.Matcher
import           Data.Pattern.Types
import           Data.Typed           hiding (label)
import           Data.Word

matchPM :: PatternMatcher -> B.ByteString -> Bool -- (Either MatchError Bool)
matchPM = match . matcher

match :: Get Bool -> B.ByteString -> Bool
match matcher bs = case runPartialGet matcher (L.fromStrict bs) 0 of
                Left _ -> False
                -- Left "f" -> False
                -- Left "not enough bytes" -> False -- Left NotEnoughData

                Right (True,bs,usedBits) -> (L.length bs==1 && shiftR (shiftL (L.head bs) usedBits) usedBits == 1) || (L.length bs==2 && usedBits==8 && L.head (L.tail bs) == 1)
                -- | L.length bs==2 -> shiftR (shiftL (L.head bs) usedBits) usedBits == 0 && L.head (L.tail bs) == 1

                -- Right (True,bs,usedBits) -> error. unwords $ ["Unexpected return from runPartialGet",show $ L.length bs,show usedBits]

-- match_  bs = runPartialGet matchAll (L.fromStrict bs) 0
-- match_ :: PatternMatcher -> B.ByteString -> Bool -- (Either MatchError Bool)
matcher :: PatternMatcher -> Get Bool
matcher = match__ . mapPM bitSplit

match__ (tt,pat) = do
  mapM_ matchPattern pat
  return True
  where
      matchPattern (MatchType t)    = matchType t
      matchPattern (MatchBits bits) = mapM_ matchBits bits
      matchBits (Bits8 n v) = do
        r <- xor v <$> getWord8 n -- getWord8 returns bits is lsb
        when (r/=0) $ fail "f"
      matchBits (Bits16 n v) = do
        r <- xor v <$> getWord16be n
        when (r/=0) $ fail "f"
      matchBits (Bits32 n v) = do
        r <- xor v <$> getWord32be n
        when (r/=0) $ fail "f"
      matchBits (Bits64 n v) = do
        r <- xor v <$> getWord64be n
        when (r/=0) $ fail "f"
      matchType t = matchTree (solve t tt)
      matchTree (BFork l r) = do
        b <- getBool
        if b then matchTree r else matchTree l
      matchTree (BCon ts) = mapM_ matchType ts
      matchTree (Skip n) = dropBits n
      -- matchTree (Skip n) | n <=8 = void $ getWord8 n

-- import Control.Monad.State.Strict
-- import Control.Monad.Catch

-- -- type M = StateT State (Either String) ()

-- data MatcherState = Matcher State {bs::B.ByteString
--                                   ,validBits::Int -- valid bits in first byte
--                                   }

-- t = runStateT (MatcherState (B.pack [129]) 0) $ getBits8 3

-- getBits8 n | n > 0 = do
--   vb <- gets validBits
--   when (vb < n) $ throwM NotEnoughData
--   shiftL (8-vb) . B.head <$> gets bs

