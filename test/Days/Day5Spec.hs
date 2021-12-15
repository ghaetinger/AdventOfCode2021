module Days.Day5Spec where

import           Days.Day5
import           Test.Hspec

exampleVectors =
  [ ((0, 9), (5, 9))
  , ((8, 0), (0, 8))
  , ((9, 4), (3, 4))
  , ((2, 2), (2, 1))
  , ((7, 0), (7, 4))
  , ((6, 4), (2, 0))
  , ((0, 9), (2, 9))
  , ((3, 4), (1, 4))
  , ((0, 0), (8, 8))
  , ((5, 5), (8, 2))
  ]

spec :: Spec
spec = do
  describe "Auxiliary Functions" $ do
    it "readFileToVectors"
      $              readFileToVectors "./res/day5/example.txt"
      `shouldReturn` exampleVectors
    it "buildLinePoints" $ do
      buildLinePoints ((2, 7), (2, 10))
        `shouldBe` [(2, 7), (2, 8), (2, 9), (2, 10)]
      buildLinePoints ((9, 7), (7, 9)) `shouldBe` [(9, 7), (8, 8), (7, 9)]
  describe "First Question" $ do
    it "Example input" $ firstQuestion "./res/day5/example.txt" `shouldReturn` 5
    it "Result" $ firstQuestion "./res/day5/input.txt" `shouldReturn` 5197
  describe "Second Question" $ do
    it "Example input"
      $              secondQuestion "./res/day5/example.txt"
      `shouldReturn` 12
    it "Result" $ secondQuestion "./res/day5/input.txt" `shouldReturn` 18605

main :: IO ()
main = hspec spec
