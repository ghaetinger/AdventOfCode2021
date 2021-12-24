{-# LANGUAGE TupleSections #-}

module Days.Day23 where

import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Set      (Set, member)
import qualified Data.Set      as Set
import           Util.MapTools

type Pod = ([Char], Char, Int)

type DPState = ([Pod], [(Char, Int)])

type DP = Map DPState Int

exits = Set.fromList [2, 4, 6, 8]

executeDP :: DPState -> DP -> Int -> Int -> (DP, Int)
executeDP state dp energy sz
  | Map.member state dp = (dp, energy + Map.findWithDefault (negate 1) state dp)
  | isStateResult state = (Map.insert state energy dp, energy)
  | otherwise = (Map.insert state val newdp, val)
  where
    generatedStates = generateNextState state sz
    (newdp, vals) =
      foldl
        (\(m, l) (s, eng) ->
           (\(ndp, v) ->
              ( ndp
              , if v == -1
                  then l
                  else (v + eng) : l))
             (executeDP s m 0 sz))
        (dp, [])
        generatedStates
    val =
      if null vals
        then -1
        else minimum vals

isStateResult :: DPState -> Bool
isStateResult (pods, q) = all (\(pls, pc, _) -> all (== pc) pls) pods && null q

generateNextState :: DPState -> Int -> [(DPState, Int)]
generateNextState state sz =
  generateNextStateQueueing state sz ++ generateNextStateDeQueueing state sz

generateNextStateQueueing :: DPState -> Int -> [(DPState, Int)]
generateNextStateQueueing (pods, q) sz =
  concatMap (\p -> possibleQueueingStates p (pods, q) sz) pods

possibleQueueingStates :: Pod -> DPState -> Int -> [(DPState, Int)]
possibleQueueingStates pod (pods, q) sz =
  if podList == [podChar, podChar, podChar, podChar] ||
     podList == [podChar, podChar, podChar] ||
     podList == [podChar, podChar] || podList == [podChar]
    then []
    else zip (map (newPodList, ) newQueues) energies
  where
    (podList, podChar, pos) = pod
    filteredPods = filter (\(_, c, _) -> c /= podChar) pods
    possiblePositions = possibleQueueingPositions pod q
    energies =
      map (\(c, p) -> calculateEnergyQueueing p c sz pod) possiblePositions
    newStateQueues = replicate (length possiblePositions) q
    newQueues = zipWith (:) possiblePositions newStateQueues
    newPodList = (tail podList, podChar, pos) : filteredPods

possibleQueueingPositions :: Pod -> [(Char, Int)] -> [(Char, Int)]
possibleQueueingPositions (c:_, _, p) ls =
  (map (c, ) . filter (\x -> not (x `member` exits))) [low .. high]
  where
    high = checkForPositions ls (p + 1) 10 minimum 11 - 1
    low = checkForPositions ls 0 (p - 1) maximum (negate 1) + 1
possibleQueueingPositions ("", _, _) _ = []

checkForPositions :: [(Char, Int)] -> Int -> Int -> ([Int] -> Int) -> Int -> Int
checkForPositions ls low high f x =
  if null filteredList
    then x
    else f filteredList
  where
    filteredList =
      (filter (\p -> p <= high && p >= low && not (p `member` exits)) . map snd)
        ls

calculateEnergyQueueing :: Int -> Char -> Int -> Pod -> Int
calculateEnergyQueueing pos c 4 ([_], _, ppos) =
  energy c * (abs (pos - ppos) + 4)
calculateEnergyQueueing pos c 4 ([_, _], _, ppos) =
  energy c * (abs (pos - ppos) + 3)
calculateEnergyQueueing pos c 4 ([_, _, _], _, ppos) =
  energy c * (abs (pos - ppos) + 2)
calculateEnergyQueueing pos c 4 ([_, _, _, _], _, ppos) =
  energy c * (abs (pos - ppos) + 1)
calculateEnergyQueueing pos c 2 ([_], _, ppos) =
  energy c * (abs (pos - ppos) + 2)
calculateEnergyQueueing pos c 2 ([_, _], _, ppos) =
  energy c * (abs (pos - ppos) + 1)

generateNextStateDeQueueing :: DPState -> Int -> [(DPState, Int)]
generateNextStateDeQueueing (pods, qs) sz =
  (foldl
     (\l state ->
        case state of
          Just s  -> s : l
          Nothing -> l)
     [] .
   map (\q -> generateNextDeQueueing pods q sz qs))
    qs

generateNextDeQueueing ::
     [Pod] -> (Char, Int) -> Int -> [(Char, Int)] -> Maybe (DPState, Int)
generateNextDeQueueing pods q sz allQ =
  if isPodValid goalPod && canGetThere q goalPos filteredQs
    then Just
           ( ((fst q : charList, fst q, goalPos) : filteredPods, filteredQs)
           , calculateEnergyDeQueueing (snd q) sz goalPod)
    else Nothing
  where
    filteredPods = filter (\(_, c, _) -> c /= fst q) pods
    [goalPod] = filter (\(_, c, _) -> c == fst q) pods
    (charList, _, goalPos) = goalPod
    filteredQs = filter (\x -> x /= q) allQ

calculateEnergyDeQueueing :: Int -> Int -> Pod -> Int
calculateEnergyDeQueueing pos 4 ([], c, ppos) =
  energy c * (abs (pos - ppos) + 4)
calculateEnergyDeQueueing pos 4 ([_], c, ppos) =
  energy c * (abs (pos - ppos) + 3)
calculateEnergyDeQueueing pos 4 ([_, _], c, ppos) =
  energy c * (abs (pos - ppos) + 2)
calculateEnergyDeQueueing pos 4 ([_, _, _], c, ppos) =
  energy c * (abs (pos - ppos) + 1)
calculateEnergyDeQueueing pos 2 ([], c, ppos) =
  energy c * (abs (pos - ppos) + 2)
calculateEnergyDeQueueing pos 2 ([_], c, ppos) =
  energy c * (abs (pos - ppos) + 1)

energy :: Char -> Int
energy 'A' = 1
energy 'B' = 10
energy 'C' = 100
energy 'D' = 1000

canGetThere :: (Char, Int) -> Int -> [(Char, Int)] -> Bool
canGetThere (_, pos) goalPos =
  not . any (\(_, x) -> x >= min pos goalPos && x <= max pos goalPos)

isPodValid :: Pod -> Bool
isPodValid (pos, c, _) = all (== c) pos
