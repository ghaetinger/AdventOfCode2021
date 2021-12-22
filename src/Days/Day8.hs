module Days.Day8 where

import           Data.List.Split
import           Data.Map        (Map, toList)
import qualified Data.Map        as Map
import           Data.Maybe
import           Data.Set        (Set, empty, fromList, intersection)

condMapQuestion1 =
  [ (1, \s _ -> length s == 2)
  , (4, \s _ -> length s == 4)
  , (7, \s _ -> length s == 3)
  , (8, \s _ -> length s == 7)
  ] :: [(Int, Set Char -> Map Int (Set Char) -> Bool)]

condMapQuestion2 =
  [ (1, \s _ -> length s == 2)
  , (4, \s _ -> length s == 4)
  , (7, \s _ -> length s == 3)
  , (8, \s _ -> length s == 7)
  , ( 6
    , \s m -> length s == 6 && length (s `intersection` lookupOrEmpty m 1) == 1)
  , ( 5
    , \s m -> length s == 5 && length (s `intersection` lookupOrEmpty m 6) == 5)
  , ( 9
    , \s m -> length s == 6 && length (s `intersection` lookupOrEmpty m 5) == 5)
  , (0, \s _ -> length s == 6)
  , ( 3
    , \s m -> length s == 5 && length (s `intersection` lookupOrEmpty m 1) == 2)
  , (2, \_ _ -> True)
  ] :: [(Int, Set Char -> Map Int (Set Char) -> Bool)]

firstQuestion :: String -> IO Int
firstQuestion filename = do
  readLines <- readFile filename
  let pairs = (map parseLine . lines) readLines
  let valmaps = map (flip getCharSetMap condMapQuestion1 . fst) pairs
  return
    (sum $
     zipWith
       (\m words -> (length . filter (equalToAnyVal m [1, 4, 7, 8])) words)
       valmaps
       (map snd pairs))

secondQuestion :: String -> IO Int
secondQuestion filename = do
  readLines <- readFile filename
  let pairs = (map parseLine . lines) readLines
  let valmaps = map (flip getCharSetMap condMapQuestion2 . fst) pairs
  return
    (sum $
     zipWith
       (\m words -> (buildNumber . map (lookupOrZero (invertmap m))) words)
       valmaps
       (map snd pairs))

invertmap :: Map Int (Set Char) -> Map (Set Char) Int
invertmap = Map.fromList . map (\(a, b) -> (b, a)) . toList

buildNumber :: [Int] -> Int
buildNumber ls =
  (sum . zipWith (\p num -> 10 ^ p * num) (reverse [0 .. length ls - 1])) ls

getCharSetMap ::
     [Set Char]
  -> [(Int, Set Char -> Map Int (Set Char) -> Bool)]
  -> Map Int (Set Char)
getCharSetMap idxs cmpls =
  fst $
  foldl
    (\(m, l) (n, f) ->
       (\sc -> (Map.insert n sc m, filter (/= sc) l)) $ getFittingCharSet l m f)
    (Map.empty, idxs)
    cmpls

equalToAnyVal :: Map Int (Set Char) -> [Int] -> Set Char -> Bool
equalToAnyVal m l sc = any ((== sc) . lookupOrEmpty m) l

getFittingCharSet ::
     [Set Char]
  -> Map Int (Set Char)
  -> (Set Char -> Map Int (Set Char) -> Bool)
  -> Set Char
getFittingCharSet words m f = (head . filter (`f` m)) words

lookupOrEmpty :: Map Int (Set Char) -> Int -> Set Char
lookupOrEmpty m idx = fromMaybe empty (Map.lookup idx m)

lookupOrZero :: Map (Set Char) Int -> Set Char -> Int
lookupOrZero m idx = fromMaybe 0 (Map.lookup idx m)

parseLine :: String -> ([Set Char], [Set Char])
parseLine = pairSetVectorToTuple . map parseWords . splitOn " | "

pairSetVectorToTuple :: [[Set Char]] -> ([Set Char], [Set Char])
pairSetVectorToTuple (a:b:_) = (a, b)
pairSetVectorToTuple _       = error "This should not happen"

parseWords :: String -> [Set Char]
parseWords = map buildCharSet . splitOn " "

buildCharSet :: [Char] -> Set Char
buildCharSet = fromList
