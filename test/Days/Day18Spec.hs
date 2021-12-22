module Days.Day18Spec where

import           Days.Day18
import           Test.Hspec

stringToFish :: String -> Snailfish
stringToFish = (`parseString` []) . filter (/= ',')

spec :: Spec
spec = do
  describe "Auxiliary Functions" $ do
    it "read snail" $ do
      parseString "[1[23]]" [] `shouldBe`
        Pair (Value 1) (Pair (Value 2) (Value 3))
      parseString "[[23]1]" [] `shouldBe`
        Pair (Pair (Value 2) (Value 3)) (Value 1)
      parseString "[[[[[98]1]2]3]4]" [] `shouldBe`
        Pair
          (Pair
             (Pair (Pair (Pair (Value 9) (Value 8)) (Value 1)) (Value 2))
             (Value 3))
          (Value 4)
      parseString "[[[[2[98]]2]3]4]" [] `shouldBe`
        Pair
          (Pair
             (Pair (Pair (Value 2) (Pair (Value 9) (Value 8))) (Value 2))
             (Value 3))
          (Value 4)
      parseString "[7[6[5[4[32]]]]]" [] `shouldBe`
        Pair
          (Value 7)
          (Pair
             (Value 6)
             (Pair (Value 5) (Pair (Value 4) (Pair (Value 3) (Value 2)))))
    it "add snails and/or reduce" $ do
      reduceUntilFinished
        (addSnailfishes
           (stringToFish "[[[[4,3],4],4],[7,[[8,4],9]]]")
           (stringToFish "[1,1]")) `shouldBe`
        stringToFish "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"
      reduceUntilFinished
        (addSnailfishes
           (stringToFish
              "[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]")
           (stringToFish "[2,9]")) `shouldBe`
        stringToFish "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]"
      reduceUntilFinished
        (addSnailfishes
           (stringToFish "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]")
           (stringToFish "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]")) `shouldBe`
        stringToFish "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"
    it "reduce" $ do
      reduce 0 (parseString "[[[[[98]1]2]3]4]" []) `shouldBe`
        (parseString "[[[[09]2]3]4]" [], Just (9, 0), True)
      reduce 0 (parseString "[7[6[5[4[32]]]]]" []) `shouldBe`
        (parseString "[7[6[5[70]]]]" [], Just (0, 2), True)
      reduce 0 (parseString "[[6[5[4[32]]]]1]" []) `shouldBe`
        (parseString "[[6[5[70]]]3]" [], Just (0, 0), True)
      reduce 0 (parseString "[[3[2[1[73]]]][6[5[4[32]]]]]" []) `shouldBe`
        (parseString "[[3[2[80]]][9[5[4[32]]]]]" [], Just (0, 0), True)
      reduce 0 (parseString "[[3[2[80]]][9[5[4[32]]]]]" []) `shouldBe`
        (parseString "[[3[2[80]]][9[5[70]]]]" [], Just (0, 2), True)
      (reduceOnlySplit . (\(a, _, _) -> a) . reduce 0)
        (parseString "[[6[5[70]]]3]" []) `shouldBe`
        (Value 0, Nothing, False)
      (reduceOnlySplit) (Pair (Value 15) (Value 16)) `shouldBe`
        (Pair (Pair (Value 7) (Value 8)) (Value 16), Nothing, True)
  describe "First Question" $ do
    it "Example input" $
      firstQuestion "./res/day18/example.txt" `shouldReturn` 4140
    it "Result" $ firstQuestion "./res/day18/input.txt" `shouldReturn` 4391
  describe "Second Question" $ do
    it "Example Input" $
      secondQuestion "./res/day18/example.txt" `shouldReturn` 3993
    it "Result" $ secondQuestion "./res/day18/input.txt" `shouldReturn` 4626

main :: IO ()
main = hspec spec
