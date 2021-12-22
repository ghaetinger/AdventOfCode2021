module Days.Day13 where

import           Data.List.Split
import           Data.Set        (Set, member)
import qualified Data.Set        as Set
import           Util.ListTools
import           Util.MapTools

type FoldOrientation = (String, Int)

type Coord = (Int, Int)

firstQuestion :: String -> IO Int
firstQuestion filename = do
  (coords, folds:_) <- readCoordinates filename
  return ((length . rmdups . map (foldCoordinate folds)) coords)

secondQuestion :: String -> IO ()
secondQuestion filename = do
  (coords, folds) <- readCoordinates filename
  let finalCoords =
        (rmdups . foldl (\cs f -> map (foldCoordinate f) cs) coords) folds
  (printCanvas (Set.fromList finalCoords) . getCanvasSize) finalCoords
  return ()

printCanvas :: Set Coord -> Coord -> IO ()
printCanvas coordSet (maxx, maxy) =
  putStr $
  foldl
    (\s y ->
       s ++
       "\n" ++
       foldl
         (\s x ->
            s ++
            (if (x, y) `member` coordSet
               then "#"
               else " "))
         ""
         [0 .. maxx])
    "\n"
    [0 .. maxy + 1]

getCanvasSize :: [Coord] -> Coord
getCanvasSize = foldl (\(maxx, maxy) (x, y) -> (max maxx x, max maxy y)) (0, 0)

foldCoordinate :: FoldOrientation -> Coord -> Coord
foldCoordinate ("x", v) (x, y)
  | x < v = (x, y)
  | otherwise = (v - (x - v), y)
foldCoordinate ("y", v) (x, y)
  | y < v = (x, y)
  | otherwise = (x, v - (y - v))

readCoordinates :: String -> IO ([Coord], [FoldOrientation])
readCoordinates filename = do
  content <- readFile filename
  let l = lines content :: [String]
  let (coords:folds:_) = splitOn [""] l
  return
    ( map ((\(x:y:_) -> (read x, read y)) . splitOn ",") coords
    , map
        ((\(axis:v:_) -> (axis, read v)) .
         splitOn "=" . head . tail . splitOn "fold along ")
        folds)
