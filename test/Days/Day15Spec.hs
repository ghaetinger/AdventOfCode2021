module Days.Day15Spec where

import           Days.Day15
import           Test.Hspec

exampleOutput =
  [ [8, 9, 1, 2, 3]
  , [9, 1, 2, 3, 4]
  , [1, 2, 3, 4, 5]
  , [2, 3, 4, 5, 6]
  , [3, 4, 5, 6, 7]
  ]

spec :: Spec
spec = do
  describe "Auxiliary Functions" $ do
    it "replicate maps correctly" $ do
      replicateCoordMap [[1, 2], [3, 4]] 2
        `shouldBe` [[1, 2, 2, 3], [3, 4, 4, 5], [2, 3, 3, 4], [4, 5, 5, 6]]
      replicateCoordMap [[8]] 5 `shouldBe` exampleOutput
  describe "First Question" $ do
    it "Example input"
      $              firstQuestion "./res/day15/example.txt"
      `shouldReturn` 40
    it "Result" $ firstQuestion "./res/day15/input.txt" `shouldReturn` 696
  describe "Second Question" $ do
    it "Example input"
      $              secondQuestion "./res/day15/example.txt"
      `shouldReturn` 315
    it "Result" $ secondQuestion "./res/day15/input.txt" `shouldReturn` 2952

main :: IO ()
main = hspec spec
