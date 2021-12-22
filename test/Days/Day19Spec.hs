module Days.Day19Spec where

import           Days.Day19
import           Test.Hspec

spec :: Spec
spec = do
  describe "Auxiliary Functions" $ do
    it "rotation" $
      rotate
        ((\(_, i, _) -> i), (\(i, _, _) -> -i), (\(_, _, i) -> i))
        (1, 0, 0) `shouldBe`
      (0, -1, 0)
  describe "First Question" $ do
    it "Example input" $
      firstQuestion "./res/day19/example.txt" `shouldReturn` 79
    it "Result" $ firstQuestion "./res/day19/input.txt" `shouldReturn` 0
  describe "Second Question" $ do
    it "Example Input" $
      secondQuestion "./res/day19/example.txt" `shouldReturn` 3621
    it "Result" $ secondQuestion "./res/day19/input.txt" `shouldReturn` 4626

main :: IO ()
main = hspec spec
