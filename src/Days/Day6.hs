module Days.Day6 where

import           Data.List.Split

initialFishes = [0, 0, 0, 0, 0, 0, 0]

initialBreededFishes = [0, 0]

firstQuestion :: String -> IO Int
firstQuestion = runQuestion 80

secondQuestion :: String -> IO Int
secondQuestion = runQuestion 256

runQuestion :: Int -> String -> IO Int
runQuestion n filename = do
  ls <- readFileIntoCountList filename
  let (finalFishes, finalBreeded) = foldl
        (\(fish, breed) _ -> updateDay fish breed)
        (ls, initialBreededFishes)
        [1 .. n]
  return (sum (finalFishes ++ finalBreeded))

updateDay :: [Int] -> [Int] -> ([Int], [Int])
updateDay (h : tail) (sev : eigh : _) = (tail ++ [sev + h], [eigh, h])
updateDay _          _                = error "This should not happen"

readFileIntoCountList :: String -> IO [Int]
readFileIntoCountList filename = do
  contents <- readFile filename
  let ls = (map read . splitOn ",") contents
  return (intListToCountList ls 0)

intListToCountList :: [Int] -> Int -> [Int]
intListToCountList _ 7 = []
intListToCountList intList n =
  (length . filter (== n)) intList : intListToCountList intList (n + 1)
