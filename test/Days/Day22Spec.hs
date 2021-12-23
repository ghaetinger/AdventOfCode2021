module Days.Day22Spec where

import           Days.Day22
import           Test.Hspec

spec :: Spec
spec = do
  describe "First Question" $ do
    it "Example input" $
      firstQuestion "./res/day22/example.txt" `shouldReturn` 590784
    it "Result" $ firstQuestion "./res/day22/input.txt" `shouldReturn` 596598
  describe "Second Question" $ do
    it "Example input" $
      secondQuestion "./res/day22/example_large.txt" `shouldReturn`
      2758514936282235
    it "Result" $
      secondQuestion "./res/day22/input.txt" `shouldReturn` 1199121349148621

main :: IO ()
main = hspec spec
