module Days.Day18 where

import           Data.List
import           Data.List.Split

data Snailfish = Pair Snailfish Snailfish | Value Int deriving (Show, Eq)

type Explosion = (Int, Int)

firstQuestion :: String -> IO Int
firstQuestion filename = do
  content <- readFile filename
  let snailFishes =
        (map ((`parseString` []) . filter (/= ',')) . lines) content :: [ Snailfish
          ]
  let finalSnailfish =
        foldl1
          (\fishFinal fish ->
            (reduceUntilFinished . addSnailfishes fishFinal) fish
          )
          snailFishes :: Snailfish
  return (sumSnailfish finalSnailfish)

secondQuestion :: String -> IO Int
secondQuestion filename = do
  content <- readFile filename
  let snailFishes =
        (map ((`parseString` []) . filter (/= ',')) . lines) content :: [ Snailfish
          ]
  let maxValues = concatMap
        (\i -> map
          (\j -> if i == j
            then -1
            else (sumSnailfish . reduceUntilFinished)
              (addSnailfishes (snailFishes !! i) (snailFishes !! j))
          )
          [0 .. (length snailFishes - 1)]
        )
        [0 .. (length snailFishes - 1)]
  return $ maximum maxValues

addSnailfishes :: Snailfish -> Snailfish -> Snailfish
addSnailfishes (Pair a b) (Pair c d) = Pair (Pair a b) (Pair c d)
addSnailfishes _          _          = error "Adding only allowed for pairs!"

reduceUntilFinished :: Snailfish -> Snailfish
reduceUntilFinished fish = case reduce 0 fish of
  (_, _, False) -> case reduceOnlySplit fish of
    (reduced, _, True ) -> reduceUntilFinished reduced
    (_      , _, False) -> fish
  (reduced, _, True) -> reduceUntilFinished reduced

sumSnailfish :: Snailfish -> Int
sumSnailfish (Pair a b) = 3 * sumSnailfish a + 2 * sumSnailfish b
sumSnailfish (Value a ) = a

reduce :: Int -> Snailfish -> (Snailfish, Maybe Explosion, Bool)
reduce 4 (Pair (Value a) (Value b)) = (Value 0, Just (a, b), True)
reduce d (Pair sa        sb       ) = case reduce (d + 1) sa of
  (sf, Nothing    , True) -> (Pair sf sb, Nothing, True)
  (sf, Just (0, 0), True) -> (Pair sf sb, Nothing, True)
  (sf, Just (l, r), True) ->
    (Pair sf (applyExplosion (0, r) sb), Just (l, 0), True)
  (_, _, False) -> case reduce (d + 1) sb of
    (sf, Nothing    , True) -> (Pair sa sf, Nothing, True)
    (sf, Just (0, 0), True) -> (Pair sa sf, Nothing, True)
    (sf, Just (l, r), True) ->
      (Pair (applyExplosion (l, 0) sa) sf, Just (0, r), True)
    (a, b, False) -> (Value 0, b, False)
reduce _ s = (s, Nothing, False)

reduceOnlySplit :: Snailfish -> (Snailfish, Maybe Explosion, Bool)
reduceOnlySplit (Value a)
  | a >= 10
  = ( Pair (Value ((floor . (/ 2) . fromIntegral) a))
           (Value ((ceiling . (/ 2) . fromIntegral) a))
    , Nothing
    , True
    )
  | otherwise
  = (Value a, Nothing, False)
reduceOnlySplit (Pair sa sb) = case reduceOnlySplit sa of
  (sf, Nothing, True ) -> (Pair sf sb, Nothing, True)
  (_ , _      , False) -> case reduceOnlySplit sb of
    (sf, Nothing, True ) -> (Pair sa sf, Nothing, True)
    (a , b      , False) -> (Value 0, b, False)

applyExplosion :: Explosion -> Snailfish -> Snailfish
applyExplosion (0, 0) s                   = s
applyExplosion (0, x) (Value a          ) = Value (a + x)
applyExplosion (0, x) (Pair (Value a) sb) = Pair (Value (a + x)) sb
applyExplosion (0, x) (Pair sa        sb) = Pair (applyExplosion (0, x) sa) sb
applyExplosion (x, 0) (Value a          ) = Value (a + x)
applyExplosion (x, 0) (Pair sa (Value b)) = Pair sa (Value (b + x))
applyExplosion (x, 0) (Pair sa sb       ) = Pair sa (applyExplosion (x, 0) sb)
applyExplosion _      s                   = s

parseString :: String -> [Snailfish] -> Snailfish
parseString ('[' : rest) st             = parseString rest st
parseString (']' : rest) (a : b : tail) = parseString rest (Pair b a : tail)
parseString (num : rest) st = parseString rest (Value (read [num]) : st)
parseString ""           (a : _)        = a
