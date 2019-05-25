{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Task5
  ( Lens
  , Lens'
  , (%~)
  , (.~)
  , (<%~)
  , (<<%~)
  , (^.)
  , _1
  , _2
  , choosing
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

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter f s = setter s <$> f (getter s)

choosing ::
     Lens s1 t1 a b -> Lens s2 t2 a b -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 _ func (Left x)  = Left <$> l1 func x
choosing _ l2 func (Right y) = Right <$> l2 func y

(<%~) :: forall s t a b. Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l func = l (duplicate . func)
  where
    duplicate x = (x, x)

(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l func = l addRes
  where
    addRes x = (x, func x)
