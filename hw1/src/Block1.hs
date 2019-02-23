module Block1
  ( contains
  , order3
  , smartReplicate
  , stringSum
  ) where

import Data.List (foldl', sort)

order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (a, b, c) = let [x, y, z] = sort [a, b, c] in (x, y, z)

smartReplicate :: [Int] -> [Int]
smartReplicate = foldMap (\x -> replicate x x)

contains :: Eq a => a -> [[a]] -> [[a]]
contains x = filter (elem x)

stringSum :: String -> Int
stringSum = foldl' (\x y -> x + read y) 0 . words
