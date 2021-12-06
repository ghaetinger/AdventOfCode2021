module Days.Day3 where

import System.IO
import Data.Char

firstQuestion :: String -> IO Int
firstQuestion filename = do
  vector <- readFileToBitVectors filename
  let gamma = buildCommonBits vector
  let epsilon = alternateBits gamma
  return (fromBinary gamma * fromBinary epsilon)

secondQuestion :: String -> IO Int
secondQuestion filename = do
  vector <- readFileToBitVectors filename
  let oxigen = filterUntilUncommon vector [] buildCommonBit
  let carbon = filterUntilUncommon vector [] (alternateBit . buildCommonBit)
  return (fromBinary oxigen * fromBinary carbon)

filterUntilUncommon :: [[Int]] -> [Int] -> ([[Int]] -> Int) -> [Int]
filterUntilUncommon [rest] word _  = word ++ rest
filterUntilUncommon words build f = (\x -> filterUntilUncommon (filterUncommon words x) (build ++ [x]) f) (f words) 

filterUncommon :: [[Int]] -> Int -> [[Int]]
filterUncommon ((a:wordTail):tail) common
  | a == common = wordTail:filterUncommon tail common
  | otherwise = filterUncommon tail common

filterUncommon list _ = []

buildCommonBits :: [[Int]] -> [Int]
buildCommonBits ([]:_) = [] 
buildCommonBits list = 
  buildCommonBit list:(buildCommonBits . cropColumn) list 

buildCommonBit :: [[Int]] -> Int
buildCommonBit list = if countOnesAtFirstPos list 0 >= (ceiling . (/2) . fromIntegral . length) list then 1 else 0 

cropColumn :: [[Int]] -> [[Int]]
cropColumn = map (\(_:tail) -> tail) 

countOnesAtFirstPos :: [[Int]] -> Int -> Int
countOnesAtFirstPos ((0:_):tail) count = countOnesAtFirstPos tail count  
countOnesAtFirstPos ((1:_):tail) count = countOnesAtFirstPos tail $ count + 1  
countOnesAtFirstPos _ count = count

alternateBits :: [Int] -> [Int]
alternateBits (a:tail) = alternateBit a:alternateBits tail
alternateBits _ = []

alternateBit :: Int -> Int
alternateBit x = -(x - 1)

fromBinary :: [Int] -> Int
fromBinary (a:tail) = (a * (2 ^ length tail)) + fromBinary tail
fromBinary _ = 0


readFileToBitVectors :: String -> IO [[Int]]
readFileToBitVectors filename = do
  contents <- readFile filename
  let concatLines = lines contents 
  return (map readCharBitsToInt concatLines)

readCharBitsToInt :: [Char] -> [Int]
readCharBitsToInt (a:tail) = digitToInt a:readCharBitsToInt tail
readCharBitsToInt _ = [] 

