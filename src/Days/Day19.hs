module Days.Day19 where

import           Data.Bifunctor
import           Data.List
import           Data.List.Split

import           Data.Map        (Map)
import qualified Data.Map        as Map

import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Util.ListTools

type Beacon = (Int, Int, Int)

type Scanner = Set Beacon

type SelectorFunction = ((Int, Int, Int) -> Int)

selectorFunctions =
  triples
    ([ (\(i, _, _) -> i)
     , (\(_, i, _) -> i)
     , (\(_, _, i) -> i)
     , (\(i, _, _) -> -i)
     , (\(_, i, _) -> -i)
     , (\(_, _, i) -> -i)
     ] :: [SelectorFunction])

firstQuestion :: String -> IO Int
firstQuestion filename = do
  scanners <- buildScanners filename
  let total = sum $ map length scanners
  let distances = map buildBeaconDistances scanners
  let intersections =
        map fst $
        filter (\((a, b), x) -> a < b && x >= 66) $
        concatMap
          (\i ->
             map
               (\j ->
                  ( (i, j)
                  , length $ Set.intersection (distances !! i) (distances !! j)))
               [0 .. (length distances - 1)])
          [0 .. (length distances - 1)] :: [(Int, Int)]
  let scannerMap =
        matchUntilEmpty scanners intersections (Map.singleton 0 (head scanners))
  let alignedMap =
        alignMaps
          scannerMap
          intersections
          (Map.singleton 0 (head scanners, (0, 0, 0)))
  return $
    length $
    foldl
      Set.union
      Set.empty
      ((map (\(_, (x, _)) -> x) . Map.toList) alignedMap)

secondQuestion :: String -> IO Int
secondQuestion filename = do
  scanners <- buildScanners filename
  let total = sum $ map length scanners
  let distances = map buildBeaconDistances scanners
  let intersections =
        map fst $
        filter (\((a, b), x) -> a < b && x >= 66) $
        concatMap
          (\i ->
             map
               (\j ->
                  ( (i, j)
                  , length $ Set.intersection (distances !! i) (distances !! j)))
               [0 .. (length distances - 1)])
          [0 .. (length distances - 1)] :: [(Int, Int)]
  let scannerMap =
        matchUntilEmpty scanners intersections (Map.singleton 0 (head scanners))
  let alignedMap =
        alignMaps
          scannerMap
          intersections
          (Map.singleton 0 (head scanners, (0, 0, 0)))
  let scanCoords = (map (\(_, (_, x)) -> x) . Map.toList) alignedMap
  return $ (maximum . map (uncurry manhattanDistance) . pairs) scanCoords

computeCoordinate :: Scanner -> Scanner -> (Scanner, (Int, Int, Int))
computeCoordinate base s = (newScanner, newCoord)
  where
    la = Set.toList base
    lb = Set.toList s
    dls =
      concatMap
        (\i ->
           map (\j -> tripleDistance (la !! i) (lb !! j)) [0 .. (length lb - 1)])
        [0 .. (length la - 1)]
    (d1, d2, d3) = mostCommonElem dls
    newScanner =
      (Set.fromList . map (\(a, b, c) -> (a + d1, b + d2, c + d3))) lb
    newCoord = (d1, d2, d3)

alignMaps ::
     Map Int Scanner
  -> [(Int, Int)]
  -> Map Int (Scanner, (Int, Int, Int))
  -> Map Int (Scanner, (Int, Int, Int))
alignMaps scanners intersections smap
  | null intersections = smap
  | otherwise = alignMaps scanners cannotIntersect newSMap
  where
    canIntersect =
      filter
        (\(ida, idb) -> Map.member ida smap || Map.member idb smap)
        intersections
    cannotIntersect =
      filter
        (\(ida, idb) -> not (Map.member ida smap || Map.member idb smap))
        intersections
    newSMap =
      foldl
        (\m (s1, s2) ->
           pairAlign
             (Map.findWithDefault Set.empty s1 scanners)
             (Map.findWithDefault Set.empty s2 scanners)
             s1
             s2
             m)
        smap
        canIntersect

matchUntilEmpty ::
     [Scanner] -> [(Int, Int)] -> Map Int Scanner -> Map Int Scanner
matchUntilEmpty scanners intersections smap
  | null intersections = smap
  | otherwise = matchUntilEmpty scanners cannotIntersect newSMap
  where
    canIntersect =
      filter
        (\(ida, idb) -> Map.member ida smap || Map.member idb smap)
        intersections
    cannotIntersect =
      filter
        (\(ida, idb) -> not (Map.member ida smap || Map.member idb smap))
        intersections
    newSMap =
      foldl
        (\m (s1, s2) -> pairRotate (scanners !! s1) (scanners !! s2) s1 s2 m)
        smap
        canIntersect

pairAlign ::
     Scanner
  -> Scanner
  -> Int
  -> Int
  -> Map Int (Scanner, (Int, Int, Int))
  -> Map Int (Scanner, (Int, Int, Int))
pairAlign a b ida idb m
  | Map.member ida m =
    let (scana, _) = Map.findWithDefault (Set.empty, (0, 0, 0)) ida m
     in Map.insert idb (computeCoordinate scana b) m
  | Map.member idb m =
    let (scanb, _) = Map.findWithDefault (Set.empty, (0, 0, 0)) idb m
     in Map.insert ida (computeCoordinate scanb a) m
  | otherwise = error "This should not happen"

pairRotate ::
     Scanner -> Scanner -> Int -> Int -> Map Int Scanner -> Map Int Scanner
pairRotate a b ida idb m
  | Map.member ida m =
    let scana = Map.findWithDefault Set.empty ida m
     in Map.insert idb (rotateUntilOk scana b selectorFunctions) m
  | Map.member idb m =
    let scanb = Map.findWithDefault Set.empty idb m
     in Map.insert ida (rotateUntilOk scanb a selectorFunctions) m
  | otherwise = error "This should not happen"

rotateUntilOk ::
     Scanner
  -> Scanner
  -> [(SelectorFunction, SelectorFunction, SelectorFunction)]
  -> Scanner
rotateUntilOk truth s [] = error ("This should not happen" ++ show (truth, s))
rotateUntilOk truth s (r:tail) =
  if checkRotationOK truth rotated
    then rotated
    else rotateUntilOk truth s tail
  where
    rotated = rotateScanner r s

rotateScanner ::
     (SelectorFunction, SelectorFunction, SelectorFunction)
  -> Scanner
  -> Scanner
rotateScanner r = Set.fromList . map (rotate r) . Set.toList

rotate ::
     (SelectorFunction, SelectorFunction, SelectorFunction) -> Beacon -> Beacon
rotate (f1, f2, f3) b = (f1 b, f2 b, f3 b)

mmult :: Num a => [[a]] -> [[a]] -> [[a]]
mmult a b = [[sum $ zipWith (*) ar bc | bc <- transpose b] | ar <- a]

checkRotationOK :: Scanner -> Scanner -> Bool
checkRotationOK sa sb = 12 <= (length . filter (== most)) dls
  where
    la = Set.toList sa
    lb = Set.toList sb
    dls =
      concatMap
        (\i -> map (\j -> (distance (la !! i) (lb !! j))) [0 .. (length lb - 1)])
        [0 .. (length la - 1)]
    most = mostCommonElem dls

buildBeaconDistances :: Scanner -> Set Float
buildBeaconDistances =
  Set.fromList . map (uncurry distance) . pairs . Set.toList

tripleDistance :: Beacon -> Beacon -> Beacon
tripleDistance (a1, a2, a3) (b1, b2, b3) = (a1 - b1, a2 - b2, a3 - b3)

distance :: Beacon -> Beacon -> Float
distance (a1, a2, a3) (b1, b2, b3) =
  (sqrt . fromIntegral) ((a1 - b1) ^ 2 + (a2 - b2) ^ 2 + (a3 - b3) ^ 2)

manhattanDistance :: Beacon -> Beacon -> Int
manhattanDistance (a1, a2, a3) (b1, b2, b3) =
  abs (a1 - b1) + abs (a2 - b2) + abs (a3 - b3)

buildScanners :: String -> IO [Scanner]
buildScanners filename = do
  content <- readFile filename
  let beaconLines =
        (map (map (map read . splitOn ",") . tail) . splitOn [""] . lines)
          content :: [[[Int]]]
  return $ map (Set.fromList . map (\[a, b, c] -> (a, b, c))) beaconLines
