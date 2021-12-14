module Days.Day8Spec where

import           Data.Set   (fromList)
import           Days.Day8
import           Test.Hspec

spec :: Spec
spec = do
  describe "Auxiliary Functions" $ do
    it "parseLine" $
      parseLine "abc def | ghi jkl" `shouldBe`
      ( [fromList ['a', 'b', 'c'], fromList ['d', 'e', 'f']]
      , [fromList ['g', 'h', 'i'], fromList ['j', 'k', 'l']])
    it "do Sets compare properly?" $ fromList "abc" `shouldBe` fromList "bca"
    it "buildNumber" $ buildNumber [1, 2, 3, 4] `shouldBe` 1234
  describe "First Question" $ do
    it "Example input" $
      firstQuestion "./res/day8/example.txt" `shouldReturn` 26
    it "Result" $ firstQuestion "./res/day8/input.txt" `shouldReturn` 255
  describe "Second Question" $ do
    it "Example input" $
      secondQuestion "./res/day8/example.txt" `shouldReturn` 61229
    it "Result" $ secondQuestion "./res/day8/input.txt" `shouldReturn` 982158

main :: IO ()
main = hspec spec
