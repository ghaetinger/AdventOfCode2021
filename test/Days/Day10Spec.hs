module Days.Day10Spec where

import           Days.Day10
import           Test.Hspec

spec :: Spec
spec = do
  describe "Auxiliary Functions" $ do
    it "parseSubsystem" $ do
      parseSubsystem "[(())]" "" `shouldBe` OK
      parseSubsystem "<[(())]>" "" `shouldBe` OK
      parseSubsystem "<[(())]>>>>>>" "" `shouldBe` OK
      parseSubsystem "[(()))" "" `shouldBe` Error ')'
      parseSubsystem "[}" "" `shouldBe` Error '}'
      parseSubsystem "<<<<<<" "" `shouldBe` Incomplete "<<<<<<"

  describe "First Question" $ do
    it "Example input"
      $              firstQuestion "./res/day10/example.txt"
      `shouldReturn` 26397
    it "Result" $ firstQuestion "./res/day10/input.txt" `shouldReturn` 323613
  describe "Second Question" $ do
    it "Example input"
      $              secondQuestion "./res/day10/example.txt"
      `shouldReturn` 288957
    it "Result"
      $              secondQuestion "./res/day10/input.txt"
      `shouldReturn` 3103006161

main :: IO ()
main = hspec spec

