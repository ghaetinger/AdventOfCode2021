module Days.Day7 where

import           Data.List
import           Data.List.Split
import           Numeric
import           Numeric.Statistics.Median

firstQuestion :: String -> IO Int
firstQuestion filename = do
  posList <- fileToPosList filename
  let med = median posList
  return ((sum . map (\pos -> round (abs (pos - med)))) posList)

secondQuestion :: String -> IO Int
secondQuestion filename = do
  posList <- fileToPosList filename
  let avgs      = getPossibleAvgValues posList
  let intVals   = map round posList
  let distances = map (calculateProgressionList intVals) avgs
  return (minimum distances)

getPossibleAvgValues :: [Float] -> [Int]
getPossibleAvgValues values =
  avgValues $ ((/ fromIntegral (length values)) . sum) values

avgValues :: Float -> [Int]
avgValues avg = [floor avg, ceiling avg]

calculateProgressionList :: [Int] -> Int -> Int
calculateProgressionList values position =
  (sum . map (sumProgression . (\x -> abs (x - position)))) values

sumProgression :: Int -> Int
sumProgression a = sum [1 .. a]

fileToPosList :: String -> IO [Float]
fileToPosList filename = do
  contents <- readFile filename
  return ((map read . splitOn ",") contents)
