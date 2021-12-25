module Days.Day25Spec where

import           Days.Day25
import           Test.Hspec

spec :: Spec
spec = do
  describe "First Question" $ do
    it "Example input" $
      firstQuestion "./res/day25/example.txt" `shouldReturn` 58
    it "Result" $ firstQuestion "./res/day25/input.txt" `shouldReturn` 571

main :: IO ()
main = hspec spec
