{-# LANGUAGE TupleSections #-}
module Util.ListTools where

import Data.Bifunctor
import           Data.List
import Data.Function (on)

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

pairs :: (Ord a)  => [a] -> [(a, a)]
pairs ls = (rmdups 
            . map (bimap (ls !!) (ls !!))
            . filter (uncurry (<))
            . concatMap (\i -> map (i,) [0 .. length ls - 1])) [0 .. length ls - 1]

triples :: [a] -> [(a, a, a)]
triples ls = (map (\(i, j, k) -> (ls !! i, ls !! j, ls !! k))
            . concatMap (\i -> concatMap (\j -> map (i,j,) [0 .. length ls - 1]) [0 .. length ls - 1])) [0 .. length ls - 1]

mostCommonElem list = fst $ maximumBy (compare `on` snd) elemCounts where
    elemCounts = nub [(element, count) | element <- list, let count = length (filter (==element) list)]
