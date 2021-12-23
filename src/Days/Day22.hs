module Days.Day22 where

import           Data.List.Split

data Toggle
  = On
  | Off
  deriving (Eq, Show)

type Range = (Toggle, (Int, Int), (Int, Int), (Int, Int))

firstQuestion :: String -> IO Int
firstQuestion filename = do
  ranges <- readRanges filename
  let filteredRanges =
        filter
          (\(_, (x1, x2), (y1, y2), (z1, z2)) ->
             all ((<= 50) . abs) [x1, x2, y1, y2, z1, z2])
          ranges
  x <- buildRangeList (filteredRanges) []
  return $ calcVolume x

secondQuestion :: String -> IO Int
secondQuestion filename = do
  ranges <- readRanges filename
  x <- buildRangeList (ranges) []
  return $ calcVolume x

calcVolume x =
  sum
    (map
       (\(_, (x1, x2), (y1, y2), (z1, z2)) ->
          (x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1))
       x)

buildRangeList :: [Range] -> [Range] -> IO [Range]
buildRangeList [] q = return q
buildRangeList (r:ranges) q = do
  case r of
    (On, _, _, _)  -> buildRangeList ranges (r : separateUntilOK r q)
    (Off, _, _, _) -> buildRangeList ranges (separateUntilOK r q)

separateUntilOK :: Range -> [Range] -> [Range]
separateUntilOK range = concatMap (`separateRanges` range)

separateRanges :: Range -> Range -> [Range]
separateRanges (t1, (minx1, maxx1), (miny1, maxy1), (minz1, maxz1)) (t2, (minx2, maxx2), (miny2, maxy2), (minz2, maxz2)) =
  if maxx < minx || maxy < miny || maxz < minz
    then [(t1, (minx1, maxx1), (miny1, maxy1), (minz1, maxz1))]
    else filter
           (\(_, (x1, x2), (y1, y2), (z1, z2)) ->
              x1 <= x2 && y1 <= y2 && z1 <= z2)
           [a, b, c, d, e, f]
  where
    (minx, maxx) = (max minx1 minx2, min maxx1 maxx2)
    (miny, maxy) = (max miny1 miny2, min maxy1 maxy2)
    (minz, maxz) = (max minz1 minz2, min maxz1 maxz2)
    (x1, x2, x3, x4) = (minx1, minx, maxx, maxx1)
    (y1, y2, y3, y4) = (miny1, miny, maxy, maxy1)
    (z1, z2, z3, z4) = (minz1, minz, maxz, maxz1)
    a = (t1, (x1, x4), (y1, y4), (z1, z2 - 1))
    b = (t1, (x1, x4), (y1, y4), (z3 + 1, z4))
    c = (t1, (x1, x2 - 1), (y1, y4), (z2, z3))
    d = (t1, (x3 + 1, x4), (y1, y4), (z2, z3))
    e = (t1, (x2, x3), (y1, y2 - 1), (z2, z3))
    f = (t1, (x2, x3), (y3 + 1, y4), (z2, z3))
    g = (t1, (minx, maxx), (miny, maxy), (minz, maxz))

readRanges :: String -> IO [Range]
readRanges filename = do
  content <- readFile filename
  return ((map parseLine . lines) content)

parseLine :: String -> Range
parseLine line = (tog, x, y, z)
  where
    [t, rest] = splitOn " " line
    tog =
      if t == "on"
        then On
        else Off
    [x, y, z] =
      (map
         ((\[a, b] -> (read a, read b)) .
          splitOn ".." .
          filter (\c -> c /= '=' && c /= 'x' && c /= 'y' && c /= 'z')) .
       splitOn ",")
        rest
