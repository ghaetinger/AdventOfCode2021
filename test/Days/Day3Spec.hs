module Days.Day3Spec where

import Test.Hspec
import Days.Day3

exampleList = [[0,0,1,0,0],[1,1,1,1,0],[1,0,1,1,0],[1,0,1,1,1],[1,0,1,0,1],[0,1,1,1,1],[0,0,1,1,1],[1,1,1,0,0],[1,0,0,0,0],[1,1,0,0,1],[0,0,0,1,0],[0,1,0,1,0]]

spec :: Spec
spec = do
  describe "Auxiliary Functions" $  do
    it "buildCommonBits" $
      buildCommonBits exampleList `shouldBe` [1, 0, 1, 1, 0] 

    it "fromBinary" $
      fromBinary [1, 0, 1, 1, 1] `shouldBe` 23  

    it "readFileToBitVectors" $
      readFileToBitVectors "./res/day3/example.txt" `shouldReturn` exampleList 

    it "filterUntilUncommon" $ 
      filterUntilUncommon exampleList [] buildCommonBit `shouldBe` [1, 0, 1, 1, 1]


  describe "First Question" $  do 
    it "Example input" $
      firstQuestion "./res/day3/example.txt" `shouldReturn` 198

    it "Result" $
      firstQuestion "./res/day3/input.txt" `shouldReturn` 2035764 

  describe "Second Question" $  do 
    it "Example input" $
      secondQuestion "./res/day3/example.txt" `shouldReturn` 230

    it "Result" $
      secondQuestion "./res/day3/input.txt" `shouldReturn` 2817661
    


main :: IO ()
main = hspec spec 
