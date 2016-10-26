{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE UndecidableInstances      #-}
module Main where

import           Control.Applicative
import           Criterion.Main
import           Criterion.Types
import           Data.Bifunctor
import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as L
import           Data.Digest.SHA3
import           Data.Foldable
import           Data.Int
import           Data.List
import qualified Data.Map              as M
import           Data.Maybe
import           Data.Model            hiding (Con)
import           Data.Pattern.Types
import qualified Data.Text             as T
import qualified Data.Text
import           Data.Typed            hiding (Con, Cons, Val, Var)
import qualified Data.Typed.PrimTypes  as P
import           Data.Word
import           Debug.Trace
import           System.Exit           (exitFailure)
import           Test.Data
import           Test.Data.Flat
import           Test.Data.Model
import qualified Test.Data2            as Data2
import qualified Test.Data3            as Data3
import           Test.Tasty
import qualified Test.Tasty            as T
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import Data.Pattern.X64
-- import Data.Pattern.Haskell

-- match = H.match

-- RUN AS: stack test --file-watch :pattern-test
t = main

-- main = mainBench
-- main = mainTest0
main = mainTest

-- FIX
-- pstr2 = patternQ


pats = mapM patternQ [[p|_|]
                      ,[p|'c'|]
                      ,[p|11|]
                      ,[p|111111|]
                      ,[p|"abc"|]
                      ,[p|"abcdefghilmnopqrstuvz"|]
                      ,[p|'a':_|]
                      ,[p|'a':_:_:_:'e':'f':'g':'h':_:_:_:_:_:_:_:'r':'s':'t':'u':'v':_|]
                      ,[p|True|]
                      ,[p|Cons False (Cons True Nil)|]
                      ,[p|Cons _ (Cons _ Nil)|]
                      ,[p|_:True:[]|]
                      ]

longList = replicate 1000 False

mainBench = do
  [wild,char,n,n2,str0,str,str1,str2,true,list,listWild,listWildS] <- pats 

  print (B.unpack $ ser (Cons False (Cons True Nil)))
  let tst proxy pat a =
        let
          at = absoluteType proxy
          -- print $ envPattern at pat
          Right pp = envPattern at pat
          mm = matcher pp
          !bs = ser a
        in bench (unwords["match",show pat,"vs",take 60 $ show a]) $ nf (match mm) bs

  defaultMainWith (defaultConfig {
                      csvFile=Just "/tmp/testReport.csv"
                      ,reportFile=Just "/tmp/testReport.html"
                      ,junitFile=Just "/tmp/testReport.xml"})
    {-        | listWild | char wild | str     |  str2 wildcards |
     Haskell  | 2 us    |            | 49 us    | 48            |
     X64      | 
   -}
    [tst (Proxy :: Proxy Char) char 'c'
    ,tst (Proxy :: Proxy String) str ("abcdefghilmnopqrstuvz"::String)
    ,tst (Proxy :: Proxy [Bool]) listWild (Cons False (Cons True Nil))
    ,tst (Proxy :: Proxy Char) wild 'c'
    ,tst (Proxy :: Proxy [Bool]) wild longList
    ,tst (Proxy :: Proxy String) str2 ("abcdefghilmnopqrstuvz"::String)
    ]

mainTest0 = do
  [wild,char,n,n2,str0,str,str1,str2,true,list,listWild,listWildS] <- pats

  let pxBool = (Proxy :: Proxy Bool)
  let pxBools = (Proxy :: Proxy (Bool,Bool,Bool))
  let pxWords = (Proxy :: Proxy (Bool,Integer,Integer,Bool))
  let pxBoolList = (Proxy :: Proxy ([Bool]))
  pfalse <- patternQ [p|False|]
  ptrue <- patternQ [p|True|]
  pbools <- patternQ [p|(True,False,True)|]
  pwords <- patternQ [p|(False,11,22,True)|]
  pstr <- patternQ [p|'a':_|]
  -- pat <- patternQ [p|Cons False (Cons _ Nil)|]
  --print $ pattern2Match at pat

  --testInstructions

  -- tst pxBoolList wild (B.unpack $ ser $ Cons False (Cons True (Cons False Nil))) True >> tst pxBoolList wild [0] False >> tst pxBoolList wild [0,1] False

  print ""
  -- mapM (\t -> t True) [
  --   tst (Proxy :: Proxy Char) char 'c'
  --   ,tst (Proxy :: Proxy String) str ("abcdefghilmnopqrstuvz"::String)
  --   ,tst (Proxy :: Proxy [Bool]) listWild (Cons False (Cons True Nil))
  --   ,tst (Proxy :: Proxy [Bool]) wild longList
  --   ,tst (Proxy :: Proxy Char) wild 'c'
  --   ,tst (Proxy :: Proxy Word8) n (11::Integer)
  --   ,tst (Proxy :: Proxy Word8) n (11::Int8)
  --   ,tst (Proxy :: Proxy Word16) wild (11::Word16)
  --   ,tst (Proxy :: Proxy String) str2 ("abcdefghilmnopqrstuvz"::String)
  --   ]
  tst (Proxy :: Proxy [Bool]) wild (Cons False (Cons True Nil)) True
  -- tst (Proxy :: Proxy String) pstr ("abcdefghilmnopqrstuvz"::String) True
  -- tst (Proxy :: Proxy String) str1 ("abcdefz"::String) True
  -- tst (Proxy :: Proxy String) str2 ("abcdefghilmnopqrstuvz"::String) True
  --tst (Proxy :: Proxy String) str1 ("ab"::String) True
  --tst (Proxy :: Proxy Char) wild (ser 'c') True
  -- tst (Proxy :: Proxy String) str (ser ("abcdefghilmnopqrstuvz"::String)) True
  -- tst (Proxy :: Proxy String) str2 (ser ("abcdefghilmnopqrstuvz"::String)) True  
  -- tst pxBool pfalse [1] True >> tst pxBool pfalse [128+1] False >> tst pxBool ptrue [1] False >> tst pxBool ptrue [128+1] True
  -- print (map X.fix8 [127,128,129])
  -- tst pxBools pbools [128+1] False >> tst pxBools pbools [128+32+1] True
  -- print $ B.unpack $ ser (44::Word16,22::Word16,33::Word16)
  -- print $ B.unpack $ ser (44::Integer,22::Integer,33::Integer)

  -- tst pxWords pwords (B.unpack $ ser (False,11::Integer,22::Integer,True)) True
  -- tst pxWords pwords (B.unpack $ ser (False,11::Integer)) False
  -- tst pxWords pwords (B.unpack $ ser (False,11::Integer,22::Integer,False)) False

    where tst proxy pat a exp = do
            let vs = ser a
            let at = absoluteType proxy
            let Right pp@(tt,mm) = envPattern at pat
            print mm
            mapM_ (\(n,c) -> print n >> print c >> putStrLn "") (M.toList tt)
            print $ (B.unpack vs,B.length vs)
            --let [MatchBits bs] = mm in print (length bs,bs)

            print $ matcherCode pp
            let r = matchPM pp vs
            -- print r
            print $ r == exp

pk = B.pack

mainTest = do
  -- let m = matchByte 11
  -- m1 <- matchG m (B.pack [44,55,88])
  -- print m1
  -- m2 <- matchG m (B.pack [11,55,88])
  -- print m2

  let proxy = Proxy :: Proxy Bool
  pat <- patternQ [p|False|]
  -- let vs = [[],[129],[1],[0],[1,1],[0,b0,0]]
  -- let proxy = Proxy :: Proxy [Bool]
  -- pat <- patternQ [p|Cons False (Cons _ Nil)|]
  let vs = [[128+32+1]]
  let at = absoluteType proxy
  --print $ pattern2Match at pat
  print $ envPattern at pat

  let Right pp = envPattern at pat
  -- print $ X.patternMatcher_ pp

  let mm = matchPM pp
  let mr = map (mm . B.pack) vs
  print "match results"
  print mr

  -- p1 <- patternQ [p|_|]
  -- p2 <- patternQ [p|'c'|]
  -- p3 <- patternQ [p|11|]
  -- p4 <- patternQ [p|111111|]
  -- p5 <- patternQ [p|"abcdefghilmnopqrstuvz"|]
  -- p6 <- patternQ [p|True|]
  -- p7 <- patternQ [p|Cons False (Cons True Nil)|]
  -- p8 <- patternQ [p|Cons _ (Cons _ Nil)|]
  ps <- pats
  T.defaultMain (tests (matchPM,ps))-- [p1,p2,p3,p4,p5,p6,p7,p8]))

-- tests :: TestTree
tests pats = testGroup "Tests" [properties
                               ,patternTests pats
                               --,matchTests
                               ]

properties = testGroup "Typed Properties" [ ]

patternTests (match,[wild,char,n,n2,str0,str,str1,str2,true,list,listWild,listWildS]) = testGroup "Pattern Tests" [
  tst wild (PVar WildCard)
  -- Should these not be named patterns as well?
  ,tst char (PVal [False,True,True,False,False,False,True,True])
  ,tst n (PVal [False,False,False,True,False,True,True,False])
  ,tst n2 (PVal [True,False,False,False,True,True,True,False ,True,True,False,False,True,False,False,False ,False,False,False,False,True,True,False,True])
  ,tst str0 (PVal [True,False,True,True,False,False,False,False,True,True,False,True,True,False,False,False,True,False,True,False,True,True,False,False,False,True,True,False])
  ,tst true (PCon "True" [])
  ,tst list (PCon "Cons" [PCon "False" [],PCon "Cons" [PCon "True" [],PCon "Nil" []]])
  ,tst listWild (PCon "Cons" [PVar WildCard,PCon "Cons" [PVar WildCard,PCon "Nil" []]])
  ,tstMatch (Proxy :: Proxy Char) wild (Right [MatchType (TypeCon (AbsRef (SHA3_256_6 7 117 93 14 24 29)))])

  ,tstMatch (Proxy :: Proxy ([Bool])) list (Right [MatchBits [True,False,True,True,False]])

  ,tstMatch (Proxy :: Proxy ([Bool])) listWild (Right [
                                                 MatchBits [True]
                                                ,MatchType (TypeCon (AbsRef (SHA3_256_6 129 212 40 48 111 29)))
                                                ,MatchBits [True]
                                                ,MatchType (TypeCon (AbsRef (SHA3_256_6 129 212 40 48 111 29)))
                                                ,MatchBits [False]])

  -- NOTE: this should fail
  --,tstMatch (Proxy :: Proxy Char) n (Right [MatchBits [True,False,True,True,False]])

  ,tstMatch (Proxy :: Proxy Char) true (Left "Constructor 'True' not present in S07755d0e181d\n")

  ,tstMatcher (Proxy :: Proxy Bool) true (B.pack [129]) True
  ,tstMatcher (Proxy :: Proxy Bool) true (B.pack [1]) False
  ,tstMatcher (Proxy :: Proxy Bool) true (B.pack [128]) False
  ,tstMatcher (Proxy :: Proxy Bool) true (B.pack [129,1]) False
  ,tstMatcher (Proxy :: Proxy Bool) true (B.pack [129,1,1]) False

  ,tstMatcher (Proxy :: Proxy [Bool]) list (ser $ Cons False (Cons True Nil)) True
  ,tstMatcher (Proxy :: Proxy [Bool]) listWild (ser $ Cons False (Cons True Nil)) True
  ,tstMatcher (Proxy :: Proxy [Bool]) listWild (ser $ Cons True (Cons False Nil)) True
  ,tstMatcher (Proxy :: Proxy [Bool]) listWild (ser $ Cons False Nil) False
  ,tstMatcher (Proxy :: Proxy [Bool]) listWildS (ser $ Cons False (Cons True Nil)) True
  ,tstMatcher (Proxy :: Proxy [Bool]) wild (ser longList) True
  ,tstMatcher (Proxy :: Proxy Char) char (ser 'c') True
  ,tstMatcher (Proxy :: Proxy Char) wild (ser 'c') True  
  ,tstMatcher (Proxy :: Proxy String) str (ser ("abcdefghilmnopqrstuvz"::String)) True
  ,tstMatcher (Proxy :: Proxy String) str1 (ser ("abcdefz"::String)) True
  ,tstMatcher (Proxy :: Proxy String) str2 (ser ("abcdefghilmnopqrstuvz"::String)) True
  ,tstMatcher (Proxy :: Proxy String) str2 (ser ("abcdZfghilmnopqrstuvz"::String)) False
  ,tstMatcher (Proxy :: Proxy Word16) n (ser (11::Integer)) True
  ,tstMatcher (Proxy :: Proxy Word16) n2 (ser (111111::Integer)) True

  ] where
  
  tst hpat pat = testCase (unwords ["Pattern",show pat]) $ hpat @?= pat

  tstMatch proxy pat match = testCase (unwords ["Match",show match]) $ pattern2Match (absoluteType proxy) pat @?= match

  tstMatcher proxy pat val exp = testCase (unwords ["Matcher",show pat,show $ B.unpack val]) $
                             let at = absoluteType proxy
                                 Right pp = envPattern at pat
                                 mm = match pp
                             in mm val @?= exp

ser = L.toStrict . flat

-- matchTests = testGroup "Match Tests" [
--   tst (Proxy :: Proxy Char) [p|_|]  []
--   ] where tst p pat matches = testCase (unwords ["Match",show inp]) $ matches @?= patternQ pat
