module Util.MapToolsSpec where

import           Test.Hspec
import           Util.MapTools

spec :: Spec
spec = do
  describe "Map changing" $ do
    it "writeValue"
      $          writeValue [[1, 2], [3, 4]] (1, 1) 10
      `shouldBe` [[1, 2], [3, 10]]
    it "accessCoordinate"
      $          accessCoordinate [[1, 2], [3, 4]] (1, 0)
      `shouldBe` 2

main :: IO ()
main = hspec spec

