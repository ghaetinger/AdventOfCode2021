module Days.Day1 where

import System.IO

firstQuestion :: String -> IO Int
firstQuestion filename = do
  levels <- readFileToIntVec filename
  return (countIncreases levels)

secondQuestion :: String -> IO Int
secondQuestion filename = do
  levels <- readFileToIntVec filename
  return ((countIncreases . slidingWindowSum) levels)

countIncreases :: [Int] -> Int 
countIncreases (a:b:tail) = (if b > a then 1 else 0) + countIncreases (b:tail)  
countIncreases levels = 0   

slidingWindowSum :: [Int] -> [Int]
slidingWindowSum (a:b:c:tail) = (a + b + c):slidingWindowSum(b:c:tail)
slidingWindowSum levels = []


readFileToIntVec :: String -> IO [Int]
readFileToIntVec filename = do
  contents <- readFile filename
  let concatLines = lines contents 
  return (map read concatLines)
   
second :: String -> Int 
second filename = -1


