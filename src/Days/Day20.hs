module Days.Day20 where

import Data.Char (digitToInt)
import Util.ListTools
import Util.MapTools

firstQuestion :: String -> IO Int
firstQuestion filename = do
  (alg, picture) <- readPicture filename
  let infiniteValues = if head alg == '#' && alg!!(length alg - 1) == '.' then ['.', '#'] else ['.', '.'] 
  let initpic = expandPictureIfNeeded picture '.'
  let finalPicture = foldl (\p x -> expandPictureIfNeeded (enhance p (infiniteValues!!x) alg) (infiniteValues!!x)) initpic [1, 0]
  return ((length . filter (== '#') . concat) finalPicture)

secondQuestion :: String -> IO Int
secondQuestion filename = do
  (alg, picture) <- readPicture filename
  let infiniteValues = if head alg == '#' && alg!!(length alg - 1) == '.' then ['.', '#'] else ['.', '.'] 
  let initpic = expandPictureIfNeeded picture '.'
  let finalPicture = foldl (\p x -> expandPictureIfNeeded (enhance p (infiniteValues!!x) alg) (infiniteValues!!x)) initpic ((concat . replicate 25) [1, 0])
  return ((length . filter (== '#') . concat) finalPicture)

enhance :: CoordMap Char -> Char -> String -> CoordMap Char
enhance picture c alg = map (\y -> map (\x -> executePatch x y c picture alg) [0 .. length (head picture) - 1]) [0 .. length picture - 1]

executePatch :: Int -> Int -> Char -> CoordMap Char -> String -> Char
executePatch x y c picture alg
  | x <= 1 || y <= 1 || x >= length (head picture) - 1 || y >= length picture - 1 = c
  | otherwise = alg !! intListToNumber binaryNum
  where
    binaryNum = map digitToInt $ concatMap (\j -> map (\i -> if accessCoordinate picture (i, j) == '.' then '0' else '1') [x - 1 .. x + 1]) [y - 1 .. y + 1]

intListToNumber :: [Int] -> Int
intListToNumber ls =
  foldl
    (\n (v, b) -> n + (v * (2 ^ b)))
    0
    (zip ls (reverse [0 .. length ls - 1]))

expandPictureIfNeeded :: CoordMap Char -> Char -> CoordMap Char
expandPictureIfNeeded picture c =
  if rightGap && leftGap && upperGap && lowerGap
    then picture
    else expandPicture picture c
  where
    rightGap = all (\row -> drop (length row - 3) row == replicate 3 c) picture
    leftGap = all (\row -> take 3 row == replicate 3 c) picture
    upperGap = all (all (== c)) (take 3 picture)
    lowerGap = all (all (== c)) (drop (length picture - 3) picture)

expandPicture :: CoordMap Char -> Char -> CoordMap Char
expandPicture picture c = ((replicate 3 completeRow ++) . (++ replicate 3 completeRow) . map ((replicate 3 c ++) . (++ replicate 3 c))) picture
  where
    completeRow = replicate 3 c ++ replicate (length (head picture)) c ++ replicate 3 c

readPicture :: String -> IO (String, CoordMap Char)
readPicture filename = do
  content <- readFile filename
  let alg : _ : pic = lines content
  return (alg, pic)
