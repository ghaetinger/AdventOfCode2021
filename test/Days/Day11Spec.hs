module Days.Day11Spec where

import           Days.Day11
import           Test.Hspec

spec :: Spec
spec = do
  describe "Auxiliar functions" $ do
    it "runUpdate" $ runUpdate ([[1, 2, 3]], 0) `shouldBe` ([[2, 3, 4]], 0)
  describe "First Question" $ do
    it "Example input"
      $              firstQuestion "./res/day11/example.txt"
      `shouldReturn` 1656
    it "Result" $ firstQuestion "./res/day11/input.txt" `shouldReturn` 1649
  describe "Second Question" $ do
    it "Example input"
      $              secondQuestion "./res/day11/example.txt"
      `shouldReturn` 195
    it "Result" $ secondQuestion "./res/day11/input.txt" `shouldReturn` 256

main :: IO ()
main = hspec spec
