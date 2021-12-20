module Days.Day17 where

import           Data.List.Split

type Point = (Int, Int)

firstQuestion :: String -> IO Int
firstQuestion filename = do
  ((minx, maxx), (miny, maxy)) <- parseInput filename
  let rminx = getX 0 minx
  let rmaxx = getX 0 maxx
  return $ executeRays (0    , 0)
                       (rminx, 1)
                       (rmaxx, 5 * abs (maxy - miny))
                       (minx , miny)
                       (maxx , maxy)

secondQuestion :: String -> IO Int
secondQuestion filename = do
  ((minx, maxx), (miny, maxy)) <- parseInput filename
  let rminx = getX 0 minx
  let rmaxx = getX 0 maxx
  return $ length
    (executeRaysAndAccum (0   , 0)
                         (-200, -200)
                         (200 , 200)
                         (minx, miny)
                         (maxx, maxy)
    )

executeRays :: Point -> Point -> Point -> Point -> Point -> Int
executeRays (x1, y1) (minx, miny) (maxx, maxy) bxmin bxmax =
  (maximum . map fst . filter snd . concatMap
      (\x -> map (\y -> runRay (x1, y1) x y 0 bxmin bxmax) [miny .. maxy])
    )
    [minx .. maxx]

executeRaysAndAccum :: Point -> Point -> Point -> Point -> Point -> [Point]
executeRaysAndAccum (x1, y1) (minx, miny) (maxx, maxy) bxmin bxmax =
  (filter (/= (-1, -1)) . concatMap
      (\x -> map
        (\y ->
          if snd (runRay (x1, y1) x y 0 bxmin bxmax) then (x, y) else (-1, -1)
        )
        [miny .. maxy]
      )
    )
    [minx .. maxx]

runRay :: Point -> Int -> Int -> Int -> Point -> Point -> (Int, Bool)
runRay (x1, y1) x y v (minx, miny) (maxx, maxy)
  | x1 > maxx || y1 < maxy = (-1, False)
  | x1 <= maxx && x1 >= minx && y1 >= maxy && y1 <= miny = (v, True)
  | otherwise = runRay (x1 + x, y1 + y)
                       (if x /= 0 then x - 1 else 0)
                       (y - 1)
                       (max v y1)
                       (minx, miny)
                       (maxx, maxy)

getX :: Int -> Int -> Int
getX x1 x2 = if x2 < x1 then -n else n
 where
  n = (round . (\d -> -0.5 + sqrt (0.25 + 2.0 * d)) . fromIntegral . abs)
    (x2 - x1)

parseInput :: String -> IO (Point, Point)
parseInput filename = do
  content <- readFile filename
  let [_, stuff] = splitOn "target area: " content
  let [x, y]     = splitOn " " stuff
  let [minx, maxx] =
        ( map read
          . splitOn ".."
          . (\[_, range] -> ((\[a, _] -> a) . splitOn ",") range)
          . splitOn "x="
          )
          x
  let [maxy, miny] =
        (map read . splitOn ".." . (\[_, range] -> range) . splitOn "y=") y
  return ((minx, maxx), (miny, maxy))

