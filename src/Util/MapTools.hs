{-# LANGUAGE TupleSections #-}
module Util.MapTools where

import           Control.Lens

type CoordMap a = [[a]]

getNeighbors :: Int -> Int -> Int -> Int -> [(Int, Int)]
getNeighbors maxx maxy y x =
  filter (inBounds maxx maxy) [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]

getNeighborsWithDiagonals :: Int -> Int -> Int -> Int -> [(Int, Int)]
getNeighborsWithDiagonals maxx maxy y x = filter
  (inBounds maxx maxy)
  [ (x    , y - 1)
  , (x    , y + 1)
  , (x - 1, y)
  , (x + 1, y)
  , (x - 1, y - 1)
  , (x - 1, y + 1)
  , (x + 1, y - 1)
  , (x + 1, y + 1)
  ]

inBounds :: Int -> Int -> (Int, Int) -> Bool
inBounds maxx maxy (x, y) = x >= 0 && x < maxx && y >= 0 && y < maxy

buildCoordinates :: Int -> Int -> [[(Int, Int)]]
buildCoordinates lengthX lengthY =
  map (\y -> map (, y) [0 .. lengthX - 1]) [0 .. lengthY - 1]

accessCoordinate :: CoordMap a -> (Int, Int) -> a
accessCoordinate m (x, y) = ((!! x) . (!! y)) m

writeValue :: CoordMap a -> (Int, Int) -> a -> CoordMap a
writeValue cmap (x, y) val = cmap & element y . element x .~ val
