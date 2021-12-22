module Days.Day17Spec where

import           Days.Day17
import           Test.Hspec

spec :: Spec
spec = do
  describe "Auxiliary Functions" $ do
    it "run ray" $ runRay (0, 0) 6 9 0 (20, -5) (30, -10) `shouldBe` (45, True)
  describe "First Question" $ do
    it "Example input" $
      firstQuestion "./res/day17/example.txt" `shouldReturn` 45
    it "Result" $ firstQuestion "./res/day17/input.txt" `shouldReturn` 17766
  describe "Second Question" $ do
    it "Example Input" $
      secondQuestion "./res/day17/example.txt" `shouldReturn` 112
    it "Result" $ secondQuestion "./res/day17/input.txt" `shouldReturn` 1733

main :: IO ()
main = hspec spec
