module Days.Day5 where

import           Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map


type Pair = (Int, Int)
type Vector = (Pair, Pair)
type CoordMap = Map Pair Int

firstQuestion :: String -> IO Int
firstQuestion filename = do
  vectors <- readFileToVectors filename
  let filteredVectors = filter (\((x0, y0), (x1, y1)) -> x0 == x1 || y0 == y1) vectors
  let dict = foldl (flip countPoints) Map.empty filteredVectors
  return (Map.foldl (\count num -> (if num >= 2 then 1 else 0) + count) 0 dict)

secondQuestion :: String -> IO Int
secondQuestion filename = do
  vectors <- readFileToVectors filename
  let dict = foldl (flip countPoints) Map.empty vectors 
  return (Map.foldl (\count num -> (if num >= 2 then 1 else 0) + count) 0 dict)
  
countPoints :: Vector -> CoordMap -> CoordMap
countPoints vector cmap = (foldl (\cmapBuf point -> Map.insertWith (+) point 1 cmapBuf) cmap . buildLinePoints) vector

buildLinePoints :: Vector -> [Pair]
buildLinePoints ((x0, y0), (x1, y1))
  | x0 /= x1 && y0 /= y1 = zip (buildNumberList x0 x1) (buildNumberList y0 y1)
  | x0 == x1 = (\lsy -> zip (replicate (length lsy) x0) lsy) (buildNumberList y0 y1) 
  | y0 == y1 = (\lsx -> zip lsx (replicate (length lsx) y0)) (buildNumberList x0 x1)
  | otherwise = error "This should not happen"

buildNumberList :: Int -> Int -> [Int]
buildNumberList a b
  | a > b = reverse [b..a]
  | otherwise = [a..b]

readFileToVectors :: String -> IO [Vector]
readFileToVectors filename = do
  contents <- readFile filename
  let concatLines = lines contents
  return (map parseLineToVector concatLines)

parseLineToVector :: String -> Vector
parseLineToVector = (\[x,y,z,w] -> ((x, y), (z, w))) . map (read :: String -> Int) . concatMap (splitOn ",") . splitOn " -> "
