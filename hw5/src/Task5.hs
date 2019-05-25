{-# LANGUAGE Rank2Types    #-}
{-# LANGUAGE TupleSections #-}

module Task5
  ( Lens
  , Lens'
  , (%~)
  , (.~)
  , (^.)
  , _1
  , _2
  , lens
  , over
  , set
  , view
  ) where

import           Data.Functor.Const    (Const (Const), getConst)
import           Data.Functor.Identity (Identity (Identity), runIdentity)

type Lens s t a b
   = forall f. Functor f =>
                 (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter f s = setter s <$> f (getter s)

view :: Lens' s a -> s -> a
view l e = getConst $ l Const e

(^.) :: s -> Lens' s a -> a
(^.) e l = view l e

over :: Lens' s a -> (a -> a) -> s -> s
over l f e = runIdentity $ l (Identity . f) e

(%~) :: Lens' s a -> (a -> a) -> s -> s
(%~) = over

set :: Lens' s a -> a -> s -> s
set l x = over l (const x)

(.~) :: Lens' s a -> a -> s -> s
(.~) = set

_1 :: Lens (a, x) (b, x) a b
_1 func (f, s) = (, s) <$> func f

_2 :: Lens (x, a) (x, b) a b
_2 func (f, s) = (f, ) <$> func s
