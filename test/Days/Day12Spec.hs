module Days.Day12Spec where

import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Days.Day12
import           Test.Hspec

exampleRoutes = Map.fromList
  [ ("fs"   , Set.fromList ["DX", "end", "he", "pj"])
  , ("he"   , Set.fromList ["DX", "WI", "RW", "fs", "pj", "zg"])
  , ("start", Set.fromList ["RW", "pj", "DX"])
  , ("pj", Set.fromList ["DX", "he", "RW", "fs", "start", "zg"])
  , ("end"  , Set.fromList ["zg"])
  , ("zg"   , Set.fromList ["sl", "pj", "RW", "he", "end"])
  , ("RW"   , Set.fromList ["he", "start", "zg", "pj"])
  , ("DX"   , Set.fromList ["fs", "he", "start", "pj"])
  , ("sl"   , Set.fromList ["zg"])
  , ("WI"   , Set.fromList ["he"])
  , ("end"  , Set.fromList ["fs", "zg"])
  ]

spec :: Spec
spec = do
  describe "Auxiliar functions" $ do
    it "readRoutes"
      $              readRoute "res/day12/example.txt"
      `shouldReturn` exampleRoutes

  describe "First Question" $ do
    it "Simple Example input"
      $              firstQuestion "./res/day12/simple_example.txt"
      `shouldReturn` 19
    it "Example input"
      $              firstQuestion "./res/day12/example.txt"
      `shouldReturn` 226
    it "Result" $ firstQuestion "./res/day12/input.txt" `shouldReturn` 4707
  describe "Second Question" $ do
    it "Simple Example input"
      $              secondQuestion "./res/day12/very_simple_example.txt"
      `shouldReturn` 36
    it "Example input"
      $              secondQuestion "./res/day12/example.txt"
      `shouldReturn` 3509
    it "Result" $ secondQuestion "./res/day12/input.txt" `shouldReturn` 130493

main :: IO ()
main = hspec spec
