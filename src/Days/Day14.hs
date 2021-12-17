module Days.Day14 where

import           Data.List
import           Data.List.Split
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map

type Rules = Map String Char

type Pairs = Map String Int

firstQuestion :: String -> IO Int
firstQuestion = runProcess 10

secondQuestion :: String -> IO Int
secondQuestion = runProcess 40

runProcess :: Int -> String -> IO Int
runProcess n filename = do
  (pairs, rules) <- readRules filename
  let pairMap = foldl (\s _ -> executeRound rules s) pairs [1 .. n]
  let occuranceTuple =
        ( sortBy (\(_, a) (_, b) -> compare b a)
          . Map.toList
          . foldl
              (\m (a : b : _, n) ->
                (Map.insertWith (+) b n . Map.insertWith (+) a n) m
              )
              Map.empty
          . Map.toList
          )
          pairMap
  return
    ( (divup . snd . head) occuranceTuple
    - (divup . snd . head . (\ls -> drop (length ls - 1) ls)) occuranceTuple
    )

divup a = ceiling $ fromIntegral a / 2.0

executeRound :: Rules -> Pairs -> Pairs
executeRound rules pairs = foldl
  (\nps (c1 : c2 : _, n) ->
    ( Map.insertWith (+) [c1, Map.findWithDefault ' ' [c1, c2] rules] n
      . Map.insertWith (+) [Map.findWithDefault ' ' (c1 : [c2]) rules, c2] n
      )
      nps
  )
  Map.empty
  (Map.toList pairs)

readRules :: String -> IO (Pairs, Rules)
readRules filename = do
  content <- readFile filename
  let ((s : _) : ruleStrs : _) = (splitOn [""] . lines) content
  return
    ( foldl (\ps (a, b) -> Map.insertWith (+) [a, b] 1 ps)
            Map.empty
            ((\st -> zip (take (length st - 1) st) (tail st)) s)
    , Map.fromList
      $ map ((\(a : b : _) -> (a, head b)) . splitOn " -> ") ruleStrs
    )
