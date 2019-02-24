module Block2
    ( mergeSort
    , removeAt
    , removeAtSafe
    ) where

import Numeric.Natural (Natural)

removeAtSafe :: [a] -> Natural -> (Maybe a, [a])
removeAtSafe []    _   = (Nothing, [])
removeAtSafe (h:t) 0   = (Just h, t)
removeAtSafe (h:t) ind = let (e, l) = removeAtSafe t (ind - 1) in (e, h:l)

removeAt :: [a] -> Natural -> (a, [a])
removeAt xs ind =
  case removeAtSafe xs ind of
    (Nothing, _)   -> error "Index out of bound"
    (Just e, rest) -> (e, rest)

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort a =
  let (left, right) = split a in merge (mergeSort left) (mergeSort right)
  where
    merge [] xs = xs
    merge xs [] = xs
    merge l@(x:xs) r@(y:ys)
      | x <= y    = x:merge xs r
      | otherwise = y:merge l ys

    split (x:y:xs) = let (left, right) = split xs in (x:left, y:right)
    split xs@[_]   = (xs, [])
    split []       = ([], [])
