module Days.Day24Spec where

import           Days.Day24
import           Test.Hspec

import qualified Data.Map   as Map

spec :: Spec
spec = do
  describe "First Question" $ do
    it "Result" $ firstQuestion `shouldReturn` 69914999975369
  describe "Second Question" $ do
    it "Result" $ secondQuestion `shouldReturn` 14911675311114

main :: IO ()
main = hspec spec
