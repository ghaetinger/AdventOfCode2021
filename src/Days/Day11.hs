module Days.Day11 where

import           Data.Char                      ( digitToInt )
import           Util.MapTools

type OctopusMap = (CoordMap Int, Int)

firstQuestion :: String -> IO Int
firstQuestion filename = do
  cmap <- readMap filename
  let (_, acc) = foldl (\m _ -> runUpdate m) (cmap, 0) [1 .. 100]
  return acc

secondQuestion :: String -> IO Int
secondQuestion filename = do
  cmap <- readMap filename
  return (updateUntilAllFlash (cmap, 0) 1)

updateUntilAllFlash :: OctopusMap -> Int -> Int
updateUntilAllFlash m it =
  (\nm -> if areAllFlashing nm then it else updateUntilAllFlash nm (it + 1))
    $ runUpdate m

areAllFlashing :: OctopusMap -> Bool
areAllFlashing = all (== 0) . concat . fst

runUpdate :: OctopusMap -> OctopusMap
runUpdate (cmap, acc) =
  (\coords -> foldl (updateCoords cleanEnergy)
                    (foldl (updateCoords incrCoord) (cmap, acc) coords)
                    coords
    )
    $ concat (buildCoordinates ((length . head) cmap) (length cmap))

updateCoords
  :: (OctopusMap -> (Int, Int) -> Int -> OctopusMap)
  -> OctopusMap
  -> (Int, Int)
  -> OctopusMap
updateCoords f (cmap, acc) coord =
  f (cmap, acc) coord (accessCoordinate cmap coord)

incrCoord :: OctopusMap -> (Int, Int) -> Int -> OctopusMap
incrCoord (cmap, acc) (x, y) val
  | val < 0 = (cmap, acc)
  | val <= 8 = (writeValue cmap (x, y) (val + 1), acc)
  | val == 9 = foldl
    (updateCoords incrCoord)
    (writeValue cmap (x, y) (negate 1), acc + 1)
    (getNeighborsWithDiagonals ((length . head) cmap) (length cmap) y x)

cleanEnergy :: OctopusMap -> (Int, Int) -> Int -> OctopusMap
cleanEnergy (cmap, acc) (x, y) val | val < 0   = (writeValue cmap (x, y) 0, acc)
                                   | otherwise = (cmap, acc)

readMap :: String -> IO (CoordMap Int)
readMap filename = do
  contents <- readFile filename
  return ((map (map digitToInt) . lines) contents)
