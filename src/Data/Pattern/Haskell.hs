module Data.Pattern.Haskell where

import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as B
import qualified Data.Map                 as M
import           Data.Pattern.Types
import           Data.Typed hiding (label)
import           Data.Word

import Control.Monad.State.Strict
import Control.Monad.Catch

data MatchError = NotEnoughData

-- type M = StateT State (Either String) ()

data MatcherState = Matcher State {bs::B.ByteString
                                  ,validBits::Int -- valid bits in first byte
                                  }

t = runStateT (MatcherState (B.pack [129]) 0) $ getBits8 3

getBits8 n | n > 0 = do
  vb <- gets validBits
  when (vb < n) $ throwM NotEnoughData
  shiftL (8-vb) . B.head <$> gets bs

-- match :: PatternMatcher -> B.ByteString -> Either String Bool
-- match (tt,pat) bs = runState do
--   mapM_ matchPattern pat
--   return $ Right True
--     where
--       matchPatter (MatchBits bits) = mapM_ matchBits bits
--       matchBits (Bits8 n v) = do
--         r <- xor <$> getBits8 n
--         when r/=0 $ fail $ Right False

--       getBits8 = return (33::Word8)
