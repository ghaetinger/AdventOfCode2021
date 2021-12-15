module Days.Day1Spec where

import           Days.Day1
import           Test.Hspec

levels = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

spec :: Spec
spec = do
  describe "Auxiliary Functions" $ do
    it "readFileToIntVec"
      $              readFileToIntVec "./res/day1/example.txt"
      `shouldReturn` levels
    it "countIncreases" $ countIncreases levels `shouldBe` 7
    it "slidingWindowSum"
      $          slidingWindowSum levels
      `shouldBe` [607, 618, 618, 617, 647, 716, 769, 792]
  describe "First Question" $ do
    it "Example input" $ firstQuestion "./res/day1/example.txt" `shouldReturn` 7
    it "Result" $ firstQuestion "./res/day1/input.txt" `shouldReturn` 1266
  describe "Second Question" $ do
    it "Example input"
      $              secondQuestion "./res/day1/example.txt"
      `shouldReturn` 5
    it "Result" $ secondQuestion "./res/day1/input.txt" `shouldReturn` 1217

main :: IO ()
main = hspec spec
