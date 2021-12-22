module Days.Day15 where

import           Data.Char
import           Data.List
import           Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as Min
import           Data.Set        (Set, member)
import qualified Data.Set        as Set
import           Util.MapTools

type PriorityData = (Int, (Int, Int))

veryHighValue = 100000

firstQuestion :: String -> IO Int
firstQuestion filename = do
  cmap <- readMap filename
  let prev = map (map (const (-1, -1))) cmap
  let dist = (writeValueLazy (0, 0) 0 . map (map (const veryHighValue))) cmap
  let (maxx, maxy) = ((length . head) cmap, length cmap)
  let q =
        (Min.fromList . map (\coord -> (accessCoordinate dist coord, coord)))
          [(1, 0), (0, 1)]
  let x = runDijkstra cmap dist maxx maxy (0, 0) q
  return $ accessCoordinate x (maxx - 1, maxy - 1)

secondQuestion :: String -> IO Int
secondQuestion filename = do
  prevcmap <- readMap filename
  let cmap = replicateCoordMap prevcmap 5
  let prev = map (map (const (-1, -1))) cmap
  let dist = (writeValueLazy (0, 0) 0 . map (map (const veryHighValue))) cmap
  let (maxx, maxy) = ((length . head) cmap, length cmap)
  let q =
        (Min.fromList . map (\coord -> (accessCoordinate dist coord, coord)))
          [(1, 0), (0, 1)]
  let x = runDijkstra cmap dist maxx maxy (0, 0) q
  return $ accessCoordinate x (maxx - 1, maxy - 1)

replicateCoordMap :: CoordMap Int -> Int -> CoordMap Int
replicateCoordMap m n =
  (replicateCoordMapVertically n .
   map
     (\r ->
        foldl
          (\fr x -> fr ++ map (increaseWhenModulo . (+ x)) r)
          []
          [0 .. (n - 1)]))
    m

replicateCoordMapVertically :: Int -> CoordMap Int -> CoordMap Int
replicateCoordMapVertically n m = concatMap (`incrCmap` m) [0 .. (n - 1)]

incrCmap :: Int -> CoordMap Int -> CoordMap Int
incrCmap n = map (map (increaseWhenModulo . (+ n)))

increaseWhenModulo n
  | n >= 10 = 1 + (n - 10)
  | otherwise = n

runDijkstra ::
     CoordMap Int
  -> CoordMap Int
  -> Int
  -> Int
  -> (Int, Int)
  -> MinQueue PriorityData
  -> CoordMap Int
runDijkstra cmap dist maxx maxy (x, y) q
  | Min.null q = dist
  | otherwise = runDijkstra cmap d maxx maxy minq filteredQ
  where
    coordValue = accessCoordinate dist (x, y)
    (d, nq) =
      foldl
        (\(_d, _nq) c ->
           let alt = coordValue + accessCoordinate cmap c
            in if alt < accessCoordinate dist c
                 then (writeValueLazy c alt _d, Min.insert (alt, c) _nq)
                 else (_d, _nq))
        (dist, q)
        (getNeighbors maxx maxy y x)
    ((_, minq), filteredQ) = Min.deleteFindMin nq

readMap :: String -> IO (CoordMap Int)
readMap filename = do
  content <- readFile filename
  return ((map (map digitToInt) . lines) content)
