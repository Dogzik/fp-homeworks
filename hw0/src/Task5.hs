module Task5
    ( Nat
    , churchMul
    , churchPlus
    , churchToInt
    , succChurch
    , zero
    ) where

type Nat a = (a -> a) -> a -> a

zero :: Nat a
zero f x = x

succChurch :: Nat a -> Nat a
succChurch n f x = f (n f x)

churchPlus :: Nat a -> Nat a -> Nat a
churchPlus n m f x = n f (m f x)

churchMul :: Nat a -> Nat a -> Nat a
churchMul n m f x = n (m f) x

churchToInt :: Nat Integer -> Integer
churchToInt n = n (+ 1) 0
