module Days.Day13Spec where

import           Days.Day13
import           Test.Hspec

coords =
  [ (6 , 10)
  , (0 , 14)
  , (9 , 10)
  , (0 , 3)
  , (10, 4)
  , (4 , 11)
  , (6 , 0)
  , (6 , 12)
  , (4 , 1)
  , (0 , 13)
  , (10, 12)
  , (3 , 4)
  , (3 , 0)
  , (8 , 4)
  , (1 , 10)
  , (2 , 14)
  , (8 , 10)
  , (9 , 0)
  ]
folds = [("y", 7), ("x", 5)]

spec :: Spec
spec = do
  describe "Auxiliar functions" $ do
    it "readCoordinates"
      $              readCoordinates "./res/day13/example.txt"
      `shouldReturn` (coords, folds)
  describe "First Question" $ do
    it "Example input"
      $              firstQuestion "./res/day13/example.txt"
      `shouldReturn` 17
    it "Result" $ firstQuestion "./res/day13/input.txt" `shouldReturn` 631
  describe "First Question" $ do
    it "Result" $ secondQuestion "./res/day13/input.txt" `shouldReturn` ()

main :: IO ()
main = hspec spec
