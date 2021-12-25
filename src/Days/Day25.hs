{-# LANGUAGE LambdaCase #-}

module Days.Day25 where

import           Util.MapTools

data SeaCocumber
  = South
  | East
  | Nope
  deriving (Eq, Show)

firstQuestion :: String -> IO Int
firstQuestion filename = do
  seaCocumbers <- readSeaCocumbers filename
  return (runStepsAndHalt seaCocumbers 1)

runStepsAndHalt :: CoordMap SeaCocumber -> Int -> Int
runStepsAndHalt s i =
  let ns = (runSouthStep . runEastStep) s
   in if ns == s
        then i
        else runStepsAndHalt ns (i + 1)

runEastStep :: CoordMap SeaCocumber -> CoordMap SeaCocumber
runEastStep s =
  foldl
    (\m y ->
       foldl
         (\nm x ->
            let nextPos = nextEastPos x y (length (head s))
             in if accessCoordinate s nextPos == Nope &&
                   accessCoordinate s (x, y) == East
                  then (writeValueLazy (x, y) Nope . writeValueLazy nextPos East)
                         nm
                  else nm)
         m
         [0 .. length (head s) - 1])
    s
    [0 .. length s - 1]

runSouthStep :: CoordMap SeaCocumber -> CoordMap SeaCocumber
runSouthStep s =
  foldl
    (\m y ->
       foldl
         (\nm x ->
            let nextPos = nextSouthPos x y (length s)
             in if accessCoordinate s nextPos == Nope &&
                   accessCoordinate s (x, y) == South
                  then (writeValueLazy (x, y) Nope .
                        writeValueLazy nextPos South)
                         nm
                  else nm)
         m
         [0 .. length (head s) - 1])
    s
    [0 .. length s - 1]

nextEastPos x y len = ((x + 1) `mod` len, y)

nextSouthPos x y len = (x, (y + 1) `mod` len)

readSeaCocumbers :: String -> IO (CoordMap SeaCocumber)
readSeaCocumbers filename = do
  content <- readFile filename
  return
    ((map
        (map
           (\case
              '>' -> East
              'v' -> South
              _   -> Nope)) .
      lines)
       content)
