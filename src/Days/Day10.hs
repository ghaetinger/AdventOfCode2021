{-# LANGUAGE LambdaCase #-}

module Days.Day10 where

import           Data.List
import           Data.Map  (Map)
import qualified Data.Map  as Map
import           Data.Set  (Set, member)
import qualified Data.Set  as Set

data ParseResult
  = Error Char
  | OK
  | Incomplete String
  deriving (Show, Eq)

charmap = Map.fromList [('<', '>'), ('(', ')'), ('{', '}'), ('[', ']')]

endingChars = Set.fromList (Map.elems charmap)

valuemap =
  Map.fromList [('>', 25137), ('}', 1197), (']', 57), (')', 3)] :: Map Char Int

complmap = Map.fromList [('>', 4), ('}', 3), (']', 2), (')', 1)] :: Map Char Int

firstQuestion :: String -> IO Int
firstQuestion filename = do
  contents <- readFile filename
  let systems = lines contents :: [String]
  return $
    (sum .
     map ((\c -> Map.findWithDefault 0 c valuemap) . unwrapError) .
     filter
       (\case
          Error _ -> True
          _       -> False) .
     map (`parseSubsystem` ""))
      systems

secondQuestion :: String -> IO Int
secondQuestion filename = do
  contents <- readFile filename
  let systems = lines contents :: [String]
  return $
    (getMiddle .
     sort .
     map
       (foldl (\acc v -> acc * 5 + v) 0 .
        map
          (\c ->
             Map.findWithDefault 0 (Map.findWithDefault '_' c charmap) complmap) .
        (\case
           Incomplete s -> s
           _            -> error "This should not happen")) .
     filter
       (\case
          Incomplete _ -> True
          _            -> False) .
     map (`parseSubsystem` ""))
      systems

getMiddle :: [Int] -> Int
getMiddle ls = ls !! round (fromIntegral (length ls) / 2.0)

unwrapError :: ParseResult -> Char
unwrapError (Error c) = c
unwrapError _         = error "This should not happen"

parseSubsystem :: String -> String -> ParseResult
parseSubsystem "" "" = OK
parseSubsystem "" st = Incomplete st
parseSubsystem (c:ctail) st
  | c `Set.member` endingChars && st == "" =
    okIfThereAreOnlyClosingFromNowOn (c : ctail)
  | c `Set.member` endingChars && Map.lookup (head st) charmap == Just c =
    parseSubsystem ctail (tail st)
  | c `Set.member` endingChars = Error c
  | otherwise = parseSubsystem ctail (c : st)

okIfThereAreOnlyClosingFromNowOn :: String -> ParseResult
okIfThereAreOnlyClosingFromNowOn str =
  if all (`Set.member` endingChars) str
    then OK
    else Error (head str)
