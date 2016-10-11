{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Data.Pattern.X86 where
import           CodeGen.X86
import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as B
import qualified Data.Map                 as M
import           Data.Pattern.Types
import           Data.Typed hiding (label)
import           Data.Word
import           Foreign

-- foreign import ccall "dynamic" callWW :: FunPtr (Int -> Int -> Ptr Word8 -> IO Bool) -> Int -> Int -> Ptr Word8 -> IO Bool
-- instance Callable (Int -> Int -> Ptr Word8 -> IO Bool) where dynCCall = callWW

foreign import ccall "dynamic" callWW :: FunPtr (Int -> Int -> Ptr Word8 -> IO Word8) -> Int -> Int -> Ptr Word8 -> IO Word8
instance Callable (Int -> Int -> Ptr Word8 -> IO Word8) where dynCCall = callWW


-- match :: Match a -> Matcher
-- match = undefined

t = do
  let m = matchByte 11
  match m (B.pack [44,55,88])


match :: (Int -> Int -> Ptr Word8 -> IO Word8) -> B.ByteString -> IO (Either String Bool)
match matcher bs =
  let (ptr,off,len) = B.toForeignPtr bs
  in retCode <$> withForeignPtr ptr (matcher off len)

retCode c | c == falseCode = Right False
          | c == trueCode = Right True
          | c== errorNotEnoughDataCode = Left "Not enough data"
          | otherwise = Left "Unknown return code"

-- arg1 offset
-- arg2 len
-- arg3 ptr
-- matchByte :: Word8 -> Code
matchByte :: Word8 -> Int -> Int -> Ptr Word8 -> IO Word8 -- Bool
matchByte b = compile $ do
  mov result arg3
  add result arg1
  -- inc result
  mov al (addr8 result)
  -- mov cl b
  traceReg "d" al
  xor_ al (fromIntegral b)
  if_ Z (mov result 1) (mov result 0)
  traceReg "d" al
  -- mov result al
  traceReg "d" result
  ret

falseCode = 0
trueCode = 1
errorNotEnoughDataCode = 2

matchPattern :: PatternMatcher -> Int -> Int -> Ptr Word8 -> IO Word8
matchPattern (tt,pat) = compile $ do
  -- xor_ al 0

  -- Inputs
  let bsOffset = arg1
  let bsLength = arg2
  let bsBase = arg3

  -- State
  let ptr = arg3
  let numBits = arg2
  -- let word = al -- lower result
  let word = addr8 ptr
  let work = cl -- lower arg4
  let work64 = arg4

  let
    compByte (Bits8 n b) = do
        --loadData

        ensureBits n

        -- mov work word
        mov work word
        and_ work (fromIntegral $ andMask 8 n)
        xor_ work (fromIntegral b)
        expect Z falseCode

        shl word (fromIntegral n)

    ensureBits n = do
      sub numBits (fromIntegral n)
      expect NC errorNotEnoughDataCode

    ensureBit = do
      dec numBits
      expect NC errorNotEnoughDataCode

    loadData = do
        mov work64 numBits
        and_ work64 7
        unless NZ $ do
          mov word (addr8 ptr)
          inc ptr

    comp (MatchBits bs) = mapM_ compByte bs

    -- TODO: ref to labelled tree
    comp (MatchType t) = jmp (solve t ls) -- compType t

    compType t = do
      l <- label
      compTree (solve t tt)
      return label

    compTree (BFork l r) = do
      ensureBit
      mov work word
      and_ work 128
      if_ Z (compTree l) (compTree r)

    compTree (BCon ts) = mapM_ compType ts

    compTree (Skip n) = ensureBits n

  --mov ptr bsBase
  add ptr bsOffset

  --mov numBits bsLength
  shl numBits 3

  traceReg "u" numBits
  traceReg "u" word

  mapM_ comp pat

  -- compile labelled decoders
  ls <- sequence_ $ M.map compType tt

  -- TODO: labelled tree

  -- check for flat closure

  mov result trueCode
  ret

expect condition onFail = unless condition (mov result onFail >> ret)
