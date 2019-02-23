{-# LANGUAGE InstanceSigs #-}

module Block4
    ( NonEmpty(..)
    , Pair(..)
    , joinWith
    , splitOn
    ) where

-- task1
import Block3 (Tree (..))
import Data.List (foldl')

data Pair a = Pair a a
data NonEmpty a = a :| [a] deriving (Show)

instance Foldable Pair where
  foldr :: (a -> b -> b) -> b -> Pair a -> b
  foldr f z (Pair x y) = f x (f y z)

  foldMap :: Monoid m => (a -> m) -> Pair a -> m
  foldMap f (Pair x y) = f x <> f y <> mempty

instance Foldable NonEmpty where
  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f z (x :| xs) = f x (foldr f z xs)

  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap f (x :| xs) = f x <> foldMap f xs

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ z Leaf          = z
  foldr f z (Node es l r) = foldr f (foldr f (foldr f z r) es) l

  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf          = mempty
  foldMap f (Node es l r) = foldMap f l <> foldMap f es <> foldMap f r

-- task2
splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn delim s = foldr folder ([] :| []) s
  where
    folder c (x :| xs)
      | c == delim = [] :| (x:xs)
      | otherwise  = (c:x) :| xs

joinWith :: a -> NonEmpty [a] -> [a]
joinWith _     (x :| []) = x
joinWith delim (x :| xs) = foldl' (\a b -> a ++ (delim:b)) x xs
