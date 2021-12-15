module Days.Day2 where

import           Data.List.Split

data Position
  = SimplePosition Int Int
  | AimPosition Int Int Int
  deriving (Show, Eq)

firstQuestion :: String -> IO Int
firstQuestion filename = do
  directions <- readFileToDirectionTupleVec filename
  let initialPosition = SimplePosition 0 0
  let SimplePosition horizontal depth =
        processFinalPosition initialPosition directions
  return (horizontal * depth)

secondQuestion :: String -> IO Int
secondQuestion filename = do
  directions <- readFileToDirectionTupleVec filename
  let initialPosition = AimPosition 0 0 0
  let AimPosition horizontal depth aim =
        processFinalPosition initialPosition directions
  return (horizontal * depth)

processFinalPosition :: Position -> [(String, Int)] -> Position
processFinalPosition pos ((dir, mag) : tail) =
  processFinalPosition (processPosition pos dir mag) tail
processFinalPosition pos _ = pos

processPosition :: Position -> String -> Int -> Position
processPosition (SimplePosition h d) "forward" mag = SimplePosition (h + mag) d
processPosition (SimplePosition h d) "up"      mag = SimplePosition h (d - mag)
processPosition (SimplePosition h d) "down"    mag = SimplePosition h (d + mag)
processPosition (AimPosition h d aim) "forward" mag =
  AimPosition (h + mag) (d + (aim * mag)) aim
processPosition (AimPosition h d aim) "up"   mag = AimPosition h d (aim - mag)
processPosition (AimPosition h d aim) "down" mag = AimPosition h d (aim + mag)

readFileToDirectionTupleVec :: String -> IO [(String, Int)]
readFileToDirectionTupleVec filename = do
  contents <- readFile filename
  let concatLines = lines contents
  return (map parseLineToTuple concatLines)

parseLineToTuple :: String -> (String, Int)
parseLineToTuple = parseTuple . splitOn " "

parseTuple :: [String] -> (String, Int)
parseTuple (dir : mag : _) = (dir, read mag)
parseTuple _               = ("", 0)
