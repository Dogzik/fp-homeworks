module Block5
  ( Endo
  , Name
  , ThisOrThat
  , eitherConcat
  , fromString
  , maybeConcat
  , toString
  ) where

import Block4 (NonEmpty (..))
import Data.List (foldl')

-- task1
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = foldMap mapper
  where
    mapper Nothing   = mempty
    mapper (Just xs) = xs

eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat = foldl' folder (mempty, mempty)
  where
    folder (lAcc, rAcc) (Left l)  = (lAcc <> l, rAcc)
    folder (lAcc, rAcc) (Right r) = (lAcc, rAcc <> r)

-- task2
instance Semigroup (NonEmpty a) where
  (x :| xs) <> (y :| ys) = x :| (xs ++ y:ys)

data ThisOrThat a b = This a | That b | Both a b

instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) where
  (This x)     <> (That y)   = Both x y
  l@(That _)   <> r@(This _) = r <> l
  (Both x y)   <> (Both a b) = Both (x <> a) (y <> b)
  (This x)     <> (This a)   = This (x <> a)
  (That y)     <> (That b)   = That (y <> b)
  (This x)     <> (Both a b) = Both (x <> a) b
  l@(Both _ _) <> r@(This _) = r <> l
  (That y)     <> (Both a b) = Both a (y <> b)
  l@(Both _ _) <> r@(That _) = r <> l

newtype Name = Name String deriving (Show, Eq)

instance Semigroup Name where
  (Name x) <> (Name y)
    | x == ""   = Name y
    | y == ""   = Name x
    | otherwise = Name $ x ++ "." ++ y

instance Monoid Name where
  mempty = Name ""

newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
  Endo { getEndo = f } <> Endo { getEndo = g } = Endo { getEndo = f . g }

instance Monoid (Endo a) where
  mempty = Endo { getEndo = id }

-- task3
data Builder = One Char | Many [Builder] deriving (Show)

instance Semigroup Builder where
  l@(One _) <> (Many []) = l
  (Many []) <> r@(One _) = r
  l@(One _) <> r@(One _) = Many [l, r]
  x@(One _) <> (Many xs) = Many $ x:xs
  (Many xs) <> x@(One _) = Many $ x:xs
  (Many xs) <> (Many ys) = Many $ xs ++ ys

instance Monoid Builder where
  mempty = Many []

fromString :: String -> Builder
fromString = foldMap One

toString :: Builder -> String
toString (One x)   = [x]
toString (Many xs) = foldMap toString xs
