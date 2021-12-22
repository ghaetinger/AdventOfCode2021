module Days.Day14Spec where

import           Days.Day14
import           Test.Hspec

spec :: Spec
spec = do
  describe "First Question" $ do
    it "Example input" $
      firstQuestion "./res/day14/example.txt" `shouldReturn` 1588
    it "Result" $ firstQuestion "./res/day14/input.txt" `shouldReturn` 3143
  describe "Second Question" $ do
    it "Example input" $
      secondQuestion "./res/day14/example.txt" `shouldReturn` 2188189693529
    it "Result" $
      secondQuestion "./res/day14/input.txt" `shouldReturn` 4110215602456

main :: IO ()
main = hspec spec
