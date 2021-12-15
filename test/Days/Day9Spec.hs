module Days.Day9Spec where

import           Days.Day9
import           Test.Hspec

import           Data.Set                       ( empty )
import qualified Data.Set                      as Set

cmap =
  [ [2, 1, 9, 9, 9, 4, 3, 2, 1, 0]
  , [3, 9, 8, 7, 8, 9, 4, 9, 2, 1]
  , [9, 8, 5, 6, 7, 8, 9, 8, 9, 2]
  , [8, 7, 6, 7, 8, 9, 6, 7, 8, 9]
  , [9, 8, 9, 9, 9, 6, 5, 6, 7, 8]
  ]

spec :: Spec
spec = do
  describe "Auxiliary Functions" $ do
    it "accessCoordinate"
      $          accessCoordinate [[1, 2], [3, 4]] (1, 0)
      `shouldBe` 2
    it "findBasin"
      $          findBasin cmap 10 5 Set.empty (6, 4)
      `shouldBe` Set.fromList
                   [ (6, 4)
                   , (7, 4)
                   , (8, 4)
                   , (9, 4)
                   , (5, 4)
                   , (6, 3)
                   , (7, 3)
                   , (8, 3)
                   , (7, 2)
                   ]
  describe "First Question" $ do
    it "Example input"
      $              firstQuestion "./res/day9/example.txt"
      `shouldReturn` 15
    it "Result" $ firstQuestion "./res/day9/input.txt" `shouldReturn` 603

  describe "Second Question" $ do
    it "Example input"
      $              secondQuestion "./res/day9/example.txt"
      `shouldReturn` 1134
    it "Result" $ secondQuestion "./res/day9/input.txt" `shouldReturn` 786780

main :: IO ()
main = hspec spec
