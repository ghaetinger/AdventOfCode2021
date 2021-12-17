module Days.Day9 where

import           Data.Char
import           Data.List
import           Data.Set                       ( Set
                                                , empty
                                                , member
                                                )
import qualified Data.Set                      as Set
import           Util.MapTools

firstQuestion :: String -> IO Int
firstQuestion filename = do
  cmap <- readValueMap filename
  let testCoords = buildTestCoordinates ((length . head) cmap) (length cmap)
  return ((sum . map (sum . map (+ 1))) (getLowestPoints cmap testCoords))

secondQuestion :: String -> IO Int
secondQuestion filename = do
  cmap <- readValueMap filename
  let maxx       = (length . head) cmap
  let maxy       = length cmap
  let testCoords = buildTestCoordinates maxx maxy
  let coords     = buildCoordinates maxx maxy
  let basins = concat $ getBasins cmap maxx maxy coords testCoords
  let bestBasins =
        ((\ls -> drop (length ls - 3) ls) . sort . map length) basins
  return (product bestBasins)

flatten :: [[[Int]]] -> [Int]
flatten = concatMap concat

getLowestPoints :: CoordMap Int -> [[[(Int, Int)]]] -> [[Int]]
getLowestPoints cmap = zipWith (zipWith (valueIfLowerThanCoords cmap)) cmap

getBasins
  :: CoordMap Int
  -> Int
  -> Int
  -> [[(Int, Int)]]
  -> [[[(Int, Int)]]]
  -> [[[Int]]]
getBasins cmap maxx maxy =
  zipWith (zipWith (basinIfLowerThanCoords cmap maxx maxy))

valueIfLowerThanCoords :: CoordMap Int -> Int -> [(Int, Int)] -> Int
valueIfLowerThanCoords cmap val coords =
  if all ((> val) . accessCoordinate cmap) coords then val else -1

basinIfLowerThanCoords
  :: CoordMap Int -> Int -> Int -> (Int, Int) -> [(Int, Int)] -> [Int]
basinIfLowerThanCoords cmap maxx maxy coord coords =
  if all ((> accessCoordinate cmap coord) . accessCoordinate cmap) coords
    then (map (accessCoordinate cmap) . Set.toList)
      (findBasin cmap maxx maxy empty coord)
    else []

buildTestCoordinates :: Int -> Int -> [[[(Int, Int)]]]
buildTestCoordinates lengthX lengthY = map
  (\y -> map (getNeighbors lengthX lengthY y) [0 .. lengthX - 1])
  [0 .. lengthY - 1]

findBasin
  :: CoordMap Int
  -> Int
  -> Int
  -> Set (Int, Int)
  -> (Int, Int)
  -> Set (Int, Int)
findBasin cmap maxx maxy explored toexplore
  | toexplore `member` explored = explored
  | accessCoordinate cmap toexplore == 9 = explored
  | otherwise = foldl (findBasin cmap maxx maxy)
                      (Set.insert toexplore explored)
                      ((\(x, y) -> getNeighbors maxx maxy y x) toexplore)

readValueMap :: String -> IO (CoordMap Int)
readValueMap filename = do
  contents <- readFile filename
  return $ (map (map digitToInt) . lines) contents
