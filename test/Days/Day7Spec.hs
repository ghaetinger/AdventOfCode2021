module Days.Day7Spec where

import           Days.Day7
import           Test.Hspec

spec :: Spec
spec = do
  describe "First Question" $ do
    it "Example input" $
      firstQuestion "./res/day7/example.txt" `shouldReturn` 37
    it "Result" $ firstQuestion "./res/day7/input.txt" `shouldReturn` 357353
  describe "Second Question" $ do
    it "Example input" $
      secondQuestion "./res/day7/example.txt" `shouldReturn` 168
    it "Result" $ secondQuestion "./res/day7/input.txt" `shouldReturn` 104822130

main :: IO ()
main = hspec spec
