module Days.Day24 where

import           Data.Char       (digitToInt)
import           Data.Foldable   (foldlM)
import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map        as Map

m1s = [10, 13, 13, -11, 11, -4, 12, 12, 15, -2, -5, -11, -13, -10]

m3s = [13, 10, 3, 1, 9, 3, 5, 1, 0, 13, 7, 15, 12, 8]

firstQuestion :: IO Int
firstQuestion = do
  let m =
        foldl
          (\m (c1, c2) -> callrunstack c1 c2 [1 .. 9] m)
          (Map.singleton 0 [])
          (zip m1s m3s)
  return $
    (read .
     reverse .
     filter (\a -> a /= '[' && a /= ']' && a /= ',') .
     show . unwrapval . Map.lookup 0)
      m

secondQuestion :: IO Int
secondQuestion = do
  let m =
        foldl
          (\m (c1, c2) -> callrunstackMin c1 c2 [1 .. 9] m)
          (Map.singleton 0 [])
          (zip m1s m3s)
  return $
    (read .
     reverse .
     filter (\a -> a /= '[' && a /= ']' && a /= ',') .
     show . unwrapval . Map.lookup 0)
      m

unwrapval :: Maybe a -> a
unwrapval (Just a) = a
unwrapval Nothing  = error "This should not happen"

callrunstack :: Int -> Int -> [Int] -> Map Int [Int] -> Map Int [Int]
callrunstack c1 c2 lsw m =
  foldl
    (\nma (z, v) ->
       foldl (\nma2 i -> Map.insert (runstack z i c1 c2) (i : v) nma2) nma lsw)
    Map.empty
    ((filter (\(z, v) -> z <= 26 ^ 5) . Map.toList) m)

callrunstackMin :: Int -> Int -> [Int] -> Map Int [Int] -> Map Int [Int]
callrunstackMin c1 c2 lsw m =
  foldl
    (\nma (z, v) ->
       foldl
         (\nma2 i ->
            Map.insertWith
              (\a b ->
                 if length a == 14 && length b == 14
                   then min a b
                   else b)
              (runstack z i c1 c2)
              (i : v)
              nma2)
         nma
         lsw)
    Map.empty
    ((filter (\(z, v) -> z <= 26 ^ 5) . Map.toList) m)

runstack :: Int -> Int -> Int -> Int -> Int
runstack z w c1 c2 = zn + (w + c2) * x
  where
    x =
      if (z `mod` 26 + c1) /= w
        then 1
        else 0
    y = (25 * x) + 1
    zn =
      (if c1 < 0
         then floor (fromIntegral z / 26.0)
         else z) *
      y
