module Days.Day6Spec where

import           Days.Day6
import           Test.Hspec

spec :: Spec
spec = do
  describe "Auxiliary Functions" $ do
    it "intListToCountList" $
      intListToCountList [1, 1, 2, 3, 4, 2] 0 `shouldBe` [0, 2, 2, 1, 1, 0, 0]
    it "runQuestion" $ runQuestion 18 "./res/day6/example.txt" `shouldReturn` 26
  describe "First Question" $ do
    it "Example input" $ do
      firstQuestion "./res/day6/example.txt" `shouldReturn` 5934
    it "Result" $ firstQuestion "./res/day6/input.txt" `shouldReturn` 375482
  describe "Second Question" $ do
    it "Example input" $
      secondQuestion "./res/day6/example.txt" `shouldReturn` 26984457539
    it "Result" $
      secondQuestion "./res/day6/input.txt" `shouldReturn` 1689540415957

main :: IO ()
main = hspec spec
