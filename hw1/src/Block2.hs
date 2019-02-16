module Block2
    ( removeAt
    , removeAtSafe
    , mergeSort
    ) where

import Data.List (length)
import Data.Maybe ()
import Numeric.Natural

removeAt :: [a] -> Natural -> (a, [a])
removeAt []    _   = error "Index out of bound"
removeAt (h:t) 0   = (h, t)
removeAt (h:t) ind = let (e, l) = removeAt t (ind - 1) in (e, h:l)

removeAtSafe :: [a] -> Natural -> (Maybe a, [a])
removeAtSafe []    _   = (Nothing, [])
removeAtSafe (h:t) 0   = (Just h, t)
removeAtSafe (h:t) ind = let (e, l) = removeAtSafe t (ind - 1) in (e, h:l)

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort (h:t) =
  let split left leftSize right rightSize
        | rightSize - leftSize <= 1 = (left, right)
        | otherwise                 = split (head right:left) (leftSize + 1) (tail right) (rightSize - 1)
  in let (left, right) = split [h] 1 t (length t)
  in merge (mergeSort left) (mergeSort right)
  where
    merge [] xs = xs
    merge xs [] = xs
    merge l@(a:at) r@(b:bt)
        | a <= b = a:merge at r
        | otherwise = b:merge l bt
