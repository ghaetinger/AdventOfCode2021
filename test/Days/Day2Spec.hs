module Days.Day2Spec where

import           Days.Day2
import           Test.Hspec

exampleDirections =
  [ ("forward", 5)
  , ("down"   , 5)
  , ("forward", 8)
  , ("up"     , 3)
  , ("down"   , 8)
  , ("forward", 2)
  ]

spec :: Spec
spec = do
  describe "Auxiliary Functions" $ do
    it "readFileToDirectionTupleVec"
      $              readFileToDirectionTupleVec "./res/day2/example.txt"
      `shouldReturn` exampleDirections
    it "processPosition"
      $          processPosition (SimplePosition 0 0) "forward" 2
      `shouldBe` SimplePosition 2 0
    it "parseLineToTuple" $ parseLineToTuple "test 2" `shouldBe` ("test", 2)
  describe "First Question" $ do
    it "Example input"
      $              firstQuestion "./res/day2/example.txt"
      `shouldReturn` 150
    it "Result" $ firstQuestion "./res/day2/input.txt" `shouldReturn` 1989014
  describe "Second Question" $ do
    it "Example input"
      $              secondQuestion "./res/day2/example.txt"
      `shouldReturn` 900
    it "Result"
      $              secondQuestion "./res/day2/input.txt"
      `shouldReturn` 2006917119

main :: IO ()
main = hspec spec
