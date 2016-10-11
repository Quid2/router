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
import           Data.Bifunctor
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as L
import           Data.Digest.SHA3
import           Data.Foldable
import           Data.Int
import           Data.List
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Model                 hiding (Con)
import           Data.Pattern.Types
import qualified Data.Text                  as T
import qualified Data.Text
import           Data.Typed                 hiding (Con, Cons, Val, Var)
import qualified Data.Typed.PrimTypes       as P
import           Data.Word
import           Debug.Trace
import           System.Exit                (exitFailure)
import           Test.Data
import           Test.Data.Flat
import           Test.Data.Model
import qualified Test.Data2                 as Data2
import qualified Test.Data3                 as Data3
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck      as QC
import Data.Pattern.X86

t = main

main = mainT

mainT = do
  let m = matchByte 11
  m1 <- match m (B.pack [44,55,88])
  print m1
  m2 <- match m (B.pack [11,55,88])
  print m2

  let proxy = Proxy :: Proxy Bool
  pat <- patternQ [p|False|]
  let proxy = Proxy :: Proxy [Bool]
  pat <- patternQ [p|Cons False (Cons _ Nil)|]
  let at = absoluteType proxy
  --print $ pattern2Match at pat
  print $ envPattern at pat

  let Right pp = envPattern at pat
  let mm = matchPattern pp
  m3 <- match mm (B.pack [])
  print "match result"
  print m3

  m4 <- match mm (B.pack [1])
  print "match result 2"
  print m4

  {-p1 <- patternQ [p|_|]
  p2 <- patternQ [p|'c'|]
  p3 <- patternQ [p|11|]
  p4 <- patternQ [p|111111|]
  p5 <- patternQ [p|"abc"|]
  p6 <- patternQ [p|True|]
  p7 <- patternQ [p|Cons False (Cons True Nil)|]
  p8 <- patternQ [p|Cons _ (Cons _ Nil)|]
  defaultMain (tests [p1,p2,p3,p4,p5,p6,p7,p8])
 -}
-- tests :: TestTree
tests pats = testGroup "Tests" [properties
                               ,patternTests pats
                               --,matchTests
                               ]

properties = testGroup "Typed Properties" [ ]

patternTests [wild,char,n,n2,str,true,list,listWild] = testGroup "Pattern Tests" [
  tst wild (Var WildCard)
  -- Should these not be named patterns as well?
  ,tst char (Val [False,True,True,False,False,False,True,True])
  ,tst n (Val [False,False,False,True,False,True,True,False])
  ,tst n2 (Val [True,False,False,False,True,True,True,False ,True,True,False,False,True,False,False,False ,False,False,False,False,True,True,False,True])
  ,tst str (Val [True,False,True,True,False,False,False,False,True,True,False,True,True,False,False,False,True,False,True,False,True,True,False,False,False,True,True,False])
  ,tst true (Con "True" [])
  ,tst list (Con "Cons" [Con "False" [],Con "Cons" [Con "True" [],Con "Nil" []]])
  ,tst listWild (Con "Cons" [Var WildCard,Con "Cons" [Var WildCard,Con "Nil" []]])
  ,tstMatch (Proxy :: Proxy Char) wild (Right [MatchType (TypeCon (AbsRef (SHA3_256_6 77 144 208 0 93 106)))])

  ,tstMatch (Proxy :: Proxy ([Bool])) list (Right [MatchBits [True,False,True,True,False]])

  ,tstMatch (Proxy :: Proxy ([Bool])) listWild (Right [
                                                 MatchBits [True]
                                                ,MatchType (TypeCon (AbsRef (SHA3_256_6 113 170 27 172 217 124)))
                                                ,MatchBits [True]
                                                ,MatchType (TypeCon (AbsRef (SHA3_256_6 113 170 27 172 217 124)))
                                                ,MatchBits [False]])

  -- NOTE: this should fail
  --,tstMatch (Proxy :: Proxy Char) n (Right [MatchBits [True,False,True,True,False]])

  ,tstMatch (Proxy :: Proxy Char) true (Left "Constructor 'True' not present in S4d90d0005d6a\n")

  ] where
  tst hpat pat = testCase (unwords ["Pattern",show pat]) $ hpat @?= pat
  tstMatch proxy pat match = testCase (unwords ["Match",show match]) $ pattern2Match (absoluteType proxy) pat @?= match

-- matchTests = testGroup "Match Tests" [
--   tst (Proxy :: Proxy Char) [p|_|]  []
--   ] where tst p pat matches = testCase (unwords ["Match",show inp]) $ matches @?= patternQ pat

