module Util.ListTools where

import           Data.List

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort
