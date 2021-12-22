module Days.Day21Spec where

import           Days.Day21
import           Test.Hspec

spec :: Spec
spec = do
--  describe "Auxiliary Functions" $ do
--      it "bogus" $ 1 `shouldBe` 1 
  describe "First Question" $ do
    it "Example input" $ firstQuestion "./res/day21/example.txt" `shouldReturn` 739785
    it "Result" $ firstQuestion "./res/day21/input.txt" `shouldReturn` 929625
  describe "Second Question" $ do
    it "Example input" $
      secondQuestion "./res/day21/example.txt" `shouldReturn` 444356092776315
    it "Result" $ secondQuestion "./res/day21/input.txt" `shouldReturn` 175731756652760

main :: IO ()
main = hspec spec
