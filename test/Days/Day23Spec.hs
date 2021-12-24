module Days.Day23Spec where

import           Days.Day23
import           Test.Hspec

import qualified Data.Map   as Map

initialStateLarge =
  ( [ (['D', 'D', 'D', 'C'], 'A', 2)
    , (['B', 'C', 'B', 'A'], 'B', 4)
    , (['C', 'B', 'A', 'D'], 'C', 6)
    , (['A', 'A', 'C', 'B'], 'D', 8)
    ]
  , [])

initialStateSmall =
  ( [ (['D', 'C'], 'A', 2)
    , (['B', 'A'], 'B', 4)
    , (['C', 'D'], 'C', 6)
    , (['A', 'B'], 'D', 8)
    ]
  , [])

spec :: Spec
spec = do
  describe "First Question" $ do
    it "Result" $
      snd (executeDP initialStateSmall Map.empty 0 2) `shouldBe` 15338
  describe "Second Question" $ do
    it "Result" $
      snd (executeDP initialStateLarge Map.empty 0 4) `shouldBe` 47064

main :: IO ()
main = hspec spec
