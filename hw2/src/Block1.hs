module Block1
  ( NonEmpty
  , Tree
  , stringSum
  ) where

import Control.Applicative (liftA2)
import Data.Monoid ((<>))
import Text.Read (readMaybe)

-- task1
stringSum :: String -> Maybe Int
stringSum s = sum <$> traverse readMaybe (words s)

--task2
data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a
  deriving (Show)

instance Functor Tree where
  fmap f (Leaf e)     = Leaf (f e)
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

instance Applicative Tree where
  pure = Leaf

  Leaf f       <*> t = f <$> t
  Branch lf rf <*> t = Branch (lf <*> t) (rf <*> t)

instance Foldable Tree where
  foldr f z (Leaf e)     = f e z
  foldr f z (Branch l r) = foldr f (foldr f z r) l

  foldMap f (Leaf e)     = f e
  foldMap f (Branch l r) = foldMap f l <> foldMap f r

instance Traversable Tree where
  traverse f (Leaf e)     = Leaf <$> f e
  traverse f (Branch l r) = liftA2 Branch (traverse f l) (traverse f r)

--task3
data NonEmpty a = a :| [a] deriving (Show)

(<|) :: NonEmpty a -> NonEmpty a -> NonEmpty a
(x :| xs) <| (y :| ys) = x :| (xs ++ (y:ys))

instance Functor NonEmpty where
  fmap f (x :| xs) = f x :| fmap f xs

instance Applicative NonEmpty where
  pure x = x :| []

  f :| []            <*> xs = f <$> xs
  f :| (fHead:fTail) <*> xs = (f <$> xs) <| (fHead :| fTail <*> xs)

instance Foldable NonEmpty where
  foldr f z (x :| xs) = f x (foldr f z xs)

  foldMap f (x :| xs) = f x <> foldMap f xs

instance Traversable NonEmpty where
  traverse f (x :| xs) = liftA2 (:|) (f x) (traverse f xs)

