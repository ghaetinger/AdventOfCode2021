module Days.Day20Spec where

import           Days.Day20
import           Test.Hspec

spec :: Spec
spec = do
  describe "First Question" $ do
    it "Example input" $ firstQuestion "./res/day20/example.txt" `shouldReturn` 35
    it "Result" $ firstQuestion "./res/day20/input.txt" `shouldReturn` 5391 
  describe "Second Question" $ do
    it "Example input" $
      secondQuestion "./res/day20/example.txt" `shouldReturn` 3351
    it "Result" $ secondQuestion "./res/day20/input.txt" `shouldReturn` 16383 

main :: IO ()
main = hspec spec
