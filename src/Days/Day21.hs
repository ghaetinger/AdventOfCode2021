module Days.Day21 where

import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Util.ListTools

type Player = (Int, Int)

type DPState = (Player, Player, Int)

firstQuestion :: String -> IO Int
firstQuestion filename = do
  (p1, p2) <- readPlayerPosition filename
  let (_, (_, lostPts), dices) = playDeterministicGame p1 p2 0
  return (lostPts * dices)

secondQuestion :: String -> IO Int
secondQuestion filename = do
  (p1, p2) <- readPlayerPosition filename
  let (m, (p1v, p2v)) = playQuantumGame p1 p2 0 Map.empty
  return (max p1v p2v)

playDeterministicGame :: Player -> Player -> Int -> (Player, Player, Int)
playDeterministicGame p1 p2 turns
  | snd p1 >= 1000 = (p1, p2, turns * 3)
  | snd p2 >= 1000 = (p1, p2, turns * 3)
  | otherwise =
    if even turns
      then let val = newPosition diceval (fst p1) in playDeterministicGame (val, snd p1 + val) p2 (turns + 1)
      else let val = newPosition diceval (fst p2) in playDeterministicGame p1 (val, snd p2 + val) (turns + 1)
  where
    diceval = (sum . map (+ (turns * 3))) [1, 2, 3]

playQuantumGame :: Player -> Player -> Int -> Map DPState (Int, Int) -> (Map DPState (Int, Int), (Int, Int))
playQuantumGame p1 p2 turns dp
  | Map.member (p1, p2, turns) dp = (dp, Map.findWithDefault (-1, -1) (p1, p2, turns) dp)
  | snd p1 >= 21 = (Map.insert (p1, p2, turns) (1, 0) dp, (1, 0))
  | snd p2 >= 21 = (Map.insert (p1, p2, turns) (0, 1) dp, (0, 1))
  | otherwise = if even turns
                   then divideUniverses (\val m -> playQuantumGame (val, snd p1 + val) p2 (turns + 1) m) (fst p1) (p1, p2, turns) dp
                   else divideUniverses (\val m -> playQuantumGame p1 (val, snd p2 + val) (turns + 1) m) (fst p2) (p1, p2, turns) dp

divideUniverses :: (Int -> Map DPState (Int, Int) -> (Map DPState (Int, Int), (Int, Int))) -> Int -> DPState -> Map DPState (Int, Int) -> (Map DPState (Int, Int), (Int, Int))
divideUniverses f pos state dp =
    (\(m, l) -> (Map.insert state l m, l))
    (foldl
        (\(m, (p1, p2)) (d1, d2, d3) -> ((\(mn, (p1n, p2n)) -> (mn, (p1 + p1n, p2 + p2n))) . f (newPosition pos (d1 + d2 + d3))) m)
        (dp, (0, 0))
        (triples [1, 2, 3])
    )

newPosition :: Int -> Int -> Int
newPosition x = (+ 1) . (`mod` 10) . (+ negate 1) . (+ x)

readPlayerPosition :: String -> IO (Player, Player)
readPlayerPosition filename = do
  content <- readFile filename
  let [one, two] = (map (read . head . tail . splitOn ": ") . lines) content :: [Int]
  return ((one, 0), (two, 0))
