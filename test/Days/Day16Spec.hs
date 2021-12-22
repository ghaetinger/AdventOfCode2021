module Days.Day16Spec where

import           Days.Day16
import           Test.Hspec

spec :: Spec
spec = do
  describe "Auxiliary Functions" $ do
    it "parse hex" $ do
      packetStringToIntList "8A00" `shouldBe` [8, 10, 0, 0]
      (map fromDecimal . packetStringToIntList) "8A00" `shouldBe`
        [[0, 0, 0, 1], [0, 1, 0, 1], [], []]
      packetStringToBitList "8A00" `shouldBe`
        [1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0]
      packetStringToBitList "38006F45291200" `shouldBe`
        [ 0
        , 0
        , 1
        , 1
        , 1
        , 0
        , 0
        , 0
        , 0
        , 0
        , 0
        , 0
        , 0
        , 0
        , 0
        , 0
        , 0
        , 1
        , 1
        , 0
        , 1
        , 1
        , 1
        , 1
        , 0
        , 1
        , 0
        , 0
        , 0
        , 1
        , 0
        , 1
        , 0
        , 0
        , 1
        , 0
        , 1
        , 0
        , 0
        , 1
        , 0
        , 0
        , 0
        , 1
        , 0
        , 0
        , 1
        , 0
        , 0
        , 0
        , 0
        , 0
        , 0
        , 0
        , 0
        , 0
        ]
      packetStringToBitList "EE00D40C823060" `shouldBe`
        [ 1
        , 1
        , 1
        , 0
        , 1
        , 1
        , 1
        , 0
        , 0
        , 0
        , 0
        , 0
        , 0
        , 0
        , 0
        , 0
        , 1
        , 1
        , 0
        , 1
        , 0
        , 1
        , 0
        , 0
        , 0
        , 0
        , 0
        , 0
        , 1
        , 1
        , 0
        , 0
        , 1
        , 0
        , 0
        , 0
        , 0
        , 0
        , 1
        , 0
        , 0
        , 0
        , 1
        , 1
        , 0
        , 0
        , 0
        , 0
        , 0
        , 1
        , 1
        , 0
        , 0
        , 0
        , 0
        , 0
        ]
    it "read Packet" $ do
      readPacket
        [1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0] `shouldBe`
        (Literal 6 2021, 21)
      (readPacket . packetStringToBitList) "38006F45291200" `shouldBe`
        (Operator 1 6 [Literal 6 10, Literal 2 20], 49)
      (readPacket . packetStringToBitList) "EE00D40C823060" `shouldBe`
        (Operator 7 3 [Literal 2 1, Literal 4 2, Literal 1 3], 51)
    it "accumId" $ do
      (accumIds . fst . readPacket . packetStringToBitList) "8A004A801A8002F478" `shouldBe`
        16
      (accumIds . fst . readPacket . packetStringToBitList)
        "620080001611562C8802118E34" `shouldBe`
        12
      (accumIds . fst . readPacket . packetStringToBitList)
        "C0015000016115A2E0802F182340" `shouldBe`
        23
      (accumIds . fst . readPacket . packetStringToBitList)
        "A0016C880162017C3686B18A3D4780" `shouldBe`
        31
  describe "First Question" $ do
    it "Example input" $
      firstQuestion "./res/day16/example.txt" `shouldReturn` 16 + 12 + 23 + 31
    it "Result" $ firstQuestion "./res/day16/input.txt" `shouldReturn` 974
  describe "Second Question" $ do
    it "Result" $
      secondQuestion "./res/day16/input.txt" `shouldReturn` 180616437720

main :: IO ()
main = hspec spec
