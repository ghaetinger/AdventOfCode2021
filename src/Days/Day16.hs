module Days.Day16 where

import           Data.Bifunctor
import           Data.List.Split
import           Numeric

data Packet = Literal Int Int | Operator Int Int [Packet] | Undefined deriving (Eq, Show)

firstQuestion :: String -> IO Int
firstQuestion filename = do
  content <- readFile filename
  let packets =
        (map (fst . readPacket . packetStringToBitList) . lines) content
  return ((sum . map accumIds) packets)

secondQuestion :: String -> IO Int
secondQuestion filename = do
  content <- readFile filename
  let packets =
        (map (fst . readPacket . packetStringToBitList) . lines) content
  return ((sum . map processPacket) packets)

accumIds :: Packet -> Int
accumIds (Literal id _)          = id
accumIds Undefined               = 0
accumIds (Operator id _ packets) = id + (sum . map accumIds) packets

processPacket :: Packet -> Int
processPacket (Literal _ n         ) = n
processPacket (Operator _ 0 packets) = (sum . map processPacket) packets
processPacket (Operator _ 1 packets) = (product . map processPacket) packets
processPacket (Operator _ 2 packets) = (minimum . map processPacket) packets
processPacket (Operator _ 3 packets) = (maximum . map processPacket) packets
processPacket (Operator _ 5 [p1, p2]) =
  if processPacket p1 > processPacket p2 then 1 else 0
processPacket (Operator _ 6 [p1, p2]) =
  if processPacket p1 < processPacket p2 then 1 else 0
processPacket (Operator _ 7 [p1, p2]) =
  if processPacket p1 == processPacket p2 then 1 else 0

readPacket :: [Int] -> (Packet, Int)
readPacket (id1 : id2 : id3 : 1 : 0 : 0 : content) =
  ( Literal (intListToNumber [id1, id2, id3])
            ((intListToNumber . concat) numPackets)
  , size
  )
 where
  size       = length numPackets * 5 + 6
  numPackets = getLiteralPacketBits content

readPacket (id1 : id2 : id3 : tp1 : tp2 : tp3 : 0 : content) =
  ( Operator (intListToNumber [id1, id2, id3])
             (intListToNumber [tp1, tp2, tp3])
             (map fst packetList)
  , len + 7 + 15
  )
 where
  packetList      = getOperatorNestedPackets ncontent len
  (len, ncontent) = (Data.Bifunctor.first intListToNumber . splitAt 15) content

readPacket (id1 : id2 : id3 : tp1 : tp2 : tp3 : 1 : content) =
  ( Operator (intListToNumber [id1, id2, id3])
             (intListToNumber [tp1, tp2, tp3])
             (map fst packetList)
  , (sum . map snd) packetList + 7 + 11
  )
 where
  packetList = getOperatorNumPacketNestedPackets ncontent numPackets
  (numPackets, ncontent) =
    (Data.Bifunctor.first intListToNumber . splitAt 11) content

readPacket x = error ("This should not happen : " ++ show x)

getOperatorNestedPackets :: [Int] -> Int -> [(Packet, Int)]
getOperatorNestedPackets _       0  = []
getOperatorNestedPackets content sz = (p, s)
  : getOperatorNestedPackets (drop s content) (sz - s)
  where (p, s) = readPacket content

getOperatorNumPacketNestedPackets :: [Int] -> Int -> [(Packet, Int)]
getOperatorNumPacketNestedPackets _ 0 = []
getOperatorNumPacketNestedPackets content n =
  (p, s) : getOperatorNumPacketNestedPackets (drop s content) (n - 1)
  where (p, s) = readPacket content

getLiteralPacketBits :: [Int] -> [[Int]]
getLiteralPacketBits (0 : b1 : b2 : b3 : b4 : _) = [[b1, b2, b3, b4]]
getLiteralPacketBits (1 : b1 : b2 : b3 : b4 : tail) =
  [b1, b2, b3, b4] : getLiteralPacketBits tail

intListToNumber :: [Int] -> Int
intListToNumber ls = foldl (\n (v, b) -> n + (v * (2 ^ b)))
                           0
                           (zip ls (reverse [0 .. length ls - 1]))

packetStringToBitList :: String -> [Int]
packetStringToBitList = concatMap paddedBinary . packetStringToIntList

paddedBinary :: Int -> [Int]
paddedBinary n = replicate (4 - length ls) 0 ++ ls
  where ls = (reverse . fromDecimal) n

fromDecimal :: Int -> [Int]
fromDecimal 0 = []
fromDecimal n = mod n 2 : fromDecimal (div n 2)

packetStringToIntList :: String -> [Int]
packetStringToIntList = map ((\((x, _) : _) -> x) . readHex . (: ""))
