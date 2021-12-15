module Days.Day4 where

import           Data.List
import           Data.List.Split

type Table = [[(Int, Bool)]]

firstQuestion :: String -> IO Int
firstQuestion filename = do
  (nums, tables) <- fileToListAndBingoTables filename
  let (lastNum, table) = evaluateTablesAndRunRound 0 nums tables
  return (lastNum * sumUnmarked table)

secondQuestion :: String -> IO Int
secondQuestion filename = do
  (nums, tables) <- fileToListAndBingoTables filename
  let (lastNum, table) = evaluateLosingTablesAndRunRound 0 nums tables
  return (lastNum * sumUnmarked table)

fileToListAndBingoTables :: String -> IO ([Int], [Table])
fileToListAndBingoTables filename = do
  contents <- readFile filename
  let concatLines                   = lines contents
  let (stringListNums : _ : tables) = concatLines
  let listNums                      = map read $ splitOn "," stringListNums
  let bingoTables                   = mountBingoTables [] tables
  return (listNums, bingoTables)

mountBingoTables :: [Table] -> [String] -> [Table]
mountBingoTables tables ("" : stringList) =
  mountBingoTables ([] : tables) stringList
mountBingoTables (table : rest) (line : stringList) =
  mountBingoTables ((readTableLine line : table) : rest) stringList
mountBingoTables [] (line : stringList) =
  mountBingoTables [[readTableLine line]] stringList
mountBingoTables table _ = table

sumUnmarked :: Table -> Int
sumUnmarked table =
  sum (map (sum . map fst . filter (not . isValueMarked)) table)

readTableLine :: String -> [(Int, Bool)]
readTableLine line =
  map (\x -> (read x, False)) $ (filter (/= "") . splitOn " ") line

evaluateTablesAndRunRound :: Int -> [Int] -> [Table] -> (Int, Table)
evaluateTablesAndRunRound prevNum (num : numList) tables =
  case find isTableComplete tables of
    Nothing -> evaluateTablesAndRunRound num numList (updateTables num tables)
    Just table -> (prevNum, table)
evaluateTablesAndRunRound _ _ _ = error "This should not happen"

evaluateLosingTablesAndRunRound :: Int -> [Int] -> [Table] -> (Int, Table)
evaluateLosingTablesAndRunRound prevNum _ [winningTable] =
  (prevNum, winningTable)
evaluateLosingTablesAndRunRound prevNum (num : numList) tables =
  evaluateLosingTablesAndRunRound num numList
    $ (updateTables num . filter (not . isTableComplete)) tables
evaluateLosingTablesAndRunRound _ _ _ = error "This should not happen"

updateTables :: Int -> [Table] -> [Table]
updateTables num = map (map (map (\(x, b) -> (x, b || x == num))))

isTableComplete :: Table -> Bool
isTableComplete table = any isLineComplete table || isAnyColumnComplete table

isAnyColumnComplete :: Table -> Bool
isAnyColumnComplete ([] : _) = False
isAnyColumnComplete lines    = (and . mapColumnMarked) lines
  || isAnyColumnComplete (map (\(_ : rest) -> rest) lines)

isLineComplete :: [(Int, Bool)] -> Bool
isLineComplete = all isValueMarked

mapColumnMarked :: Table -> [Bool]
mapColumnMarked = map (isValueMarked . head)

isValueMarked :: (Int, Bool) -> Bool
isValueMarked (_, b) = b
