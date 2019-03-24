{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module MonadInstances.FishToMonad
  ( Monad (..)
  , MonadFish (..)
  ) where

import Prelude hiding (Monad, return, (>>=))

class MonadFish m where
  returnFish :: a -> m a
  (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)

class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  return :: a -> m a

instance MonadFish m => Monad m where
  return = returnFish
  a >>= b = (id >=> b) a

