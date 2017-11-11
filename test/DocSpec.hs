module Main where
import           Data.List            (isSuffixOf)
import           Debug.Trace
import           System.FilePath.Find
import           Test.DocTest

main :: IO ()
main =
  find always ((extension ==? ".hs") &&? (exceptFiles ["Data/Pattern/Bits.hs","Data/Pattern/X64.hs"])) "src" >>= doctest

exceptFiles :: Foldable t => t [Char] -> FindClause Bool
exceptFiles mdls =
  -- let excludes = liftOp (\fp mdls -> not $ any (\mdl -> isSuffixOf mdl (traceShowId fp)) mdls)
  let excludes = liftOp (\fp mdls -> not $ any (\mdl -> isSuffixOf mdl fp) mdls)
  in filePath `excludes` mdls

