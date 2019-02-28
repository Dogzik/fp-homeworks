module Task4
    ( factorial
    , fibonacci
    , iterateElement
    , mapFix
    ) where

import Data.Function (fix)

iterateElement :: a -> [a]
iterateElement x = fix (x:)

fibonacci :: Integer -> Integer
fibonacci = fix $ \f x -> if x <= 1 then 1 else f (x - 1) + f (x - 2)

factorial :: Integer -> Integer
factorial = fix $ \f x -> if x <= 0 then 1 else x * f (x - 1)

mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix $ \f mapper list ->
                 case list of
                   []   -> []
                   x:xs -> mapper x:f mapper xs
