{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

module Task2
  ( MonadJS(..)
  , VarJS(..)
  , runSTJS
  , runSTJS1
  , f1
  , f2
  , f3
  , f4
  ) where

import           Control.Applicative (liftA2)
import           Control.Monad       (when)
import           Control.Monad.ST    (ST, runST)
import           Data.Semigroup      (Semigroup, (<>))
import           Data.STRef          (STRef, newSTRef, readSTRef, writeSTRef)

data Number
  = Inter Int
  | Frac Double
  deriving (Show)

instance Semigroup Number where
  Inter x <> Inter y = Inter $ x + y
  Frac x <> Frac y = Frac $ x + y
  Frac x <> Inter y = Frac $ x + fromIntegral y
  a <> b = b <> a

instance Eq Number where
  Inter x == Inter y = x == y
  Frac x == Frac y = x == y
  Inter x == Frac y = fromIntegral x == y
  x == y = y == x

instance Ord Number where
  Inter x <= Inter y = x <= y
  Frac x <= Frac y = x <= y
  Inter x <= Frac y = fromIntegral x <= y
  Frac x <= Inter y = x <= fromIntegral y

instance Num Number where
  Inter x + Inter y = Inter $ x + y
  Frac x + Frac y = Frac $ x + y
  Frac x + Inter y = Frac $ x + fromIntegral y
  a + b = b + a
  negate (Frac a)  = Frac $ negate a
  negate (Inter a) = Inter $ negate a
  fromInteger x = Frac $ fromInteger x
  signum (Frac a)  = Frac $ signum a
  signum (Inter a) = Inter $ signum a
  abs (Frac a)  = Frac $ abs a
  abs (Inter a) = Inter $ abs a
  Inter x * Inter y = Inter $ x * y
  Frac x * Frac y = Frac $ x * y
  Frac x * Inter y = Frac $ x * fromIntegral y
  a * b = b * a

data Val
  = Boolean Bool
  | Str String
  | Numb Number
  deriving (Show)

add :: Val -> Val -> Val
add (Boolean a) (Boolean b) = Boolean $ a || b
add (Str a) (Str b)         = Str $ a <> b
add (Numb a) (Numb b)       = Numb $ a + b
add _ _                     = Str mempty

sub :: Val -> Val -> Val
sub (Boolean a) (Boolean b) = Boolean $ (a || b) && (not a || not b) --xor
sub (Str a) (Str b)         = Str $ b <> a
sub (Numb a) (Numb b)       = Numb $ a - b
sub _ _                     = Str mempty

greater :: Val -> Val -> Bool
greater (Boolean a) (Boolean b) = a > b
greater (Str a) (Str b)         = a > b
greater (Numb a) (Numb b)       = a > b
greater _ _                     = False

class ToVal a where
  toVal :: a -> Val

instance ToVal Bool where
  toVal = Boolean

instance ToVal Int where
  toVal = Numb . Inter

instance ToVal Double where
  toVal = Numb . Frac

instance ToVal String where
  toVal = Str

class Monad m =>
      MonadJS m s
  | m -> s
  where
  type Var m a :: *
  newVar :: a -> m (Var m a)
  writeVar :: Var m a -> a -> m ()
  (@>@) :: m Val -> m Val -> m Bool
  (@#) :: m () -> m a -> m a
  (@+@) :: m Val -> m Val -> m Val
  (@-@) :: m Val -> m Val -> m Val
  (@=@) :: m (Var m Val) -> m Val -> m ()
  sReadVar :: m (Var m Val) -> m Val
  sIf :: m Bool -> m () -> m () -> m ()
  sFun1 :: (m Val -> m (Var m Val) -> m ()) -> m Val -> m Val
  sWhile :: m Bool -> m () -> m ()

class VarJS a where
  sWithVar :: MonadJS m s => a -> (m (Var m Val) -> m ()) -> m ()
  (@=) :: MonadJS m s => m (Var m Val) -> a -> m ()

instance MonadJS (ST s) s where
  type Var (ST s) a = STRef s a
  newVar = newSTRef
  writeVar = writeSTRef
  (@>@) = liftA2 greater
  (@#) = (>>)
  (@+@) = liftA2 add
  (@-@) = liftA2 sub
  (@=@) mRef mX = do
    x <- mX
    ref <- mRef
    writeSTRef ref x
  sReadVar mVar = do
    var <- mVar
    readSTRef var
  sIf cond a b = do
    flag <- cond
    if flag
      then a
      else b
  sFun1 cont arg = do
    resRef <- newSTRef $ Str ""
    cont arg $ return resRef
    readSTRef resRef
  sWhile cond body = do
    flag <- cond
    when flag $ body >> sWhile cond body

instance VarJS Val where
  sWithVar x cont = do
    ref <- newVar x
    cont (return ref)
  (@=) mRef x = do
    ref <- mRef
    writeVar ref x

sWithVarGeneric ::
     (MonadJS m s, ToVal a) => a -> (m (Var m Val) -> m ()) -> m ()
sWithVarGeneric x = sWithVar $ toVal x

assignGeneric :: (MonadJS m s, ToVal a) => m (Var m Val) -> a -> m ()
assignGeneric mRef x = mRef @= toVal x

instance VarJS Int where
  sWithVar = sWithVarGeneric
  (@=) = assignGeneric

instance VarJS Double where
  sWithVar = sWithVarGeneric
  (@=) = assignGeneric

instance VarJS Bool where
  sWithVar = sWithVarGeneric
  (@=) = assignGeneric

instance VarJS String where
  sWithVar = sWithVarGeneric
  (@=) = assignGeneric

runSTJS :: (forall s. ST s ()) -> ()
runSTJS = runST

runSTJS1 :: ToVal a => a -> (forall s. ST s Val -> ST s Val) -> Val
runSTJS1 x script = runST $ script (return $ toVal x)

f1 :: MonadJS m s => m Val -> m Val
f1 = sFun1 $ \a res -> res @=@ a

f2 :: MonadJS m s => m ()
f2 = sWithVar (toVal "keke") $ \x -> x @= toVal False

f3 :: MonadJS m s => m Val -> m Val
f3 =
  sFun1 $ \x res ->
    sWithVar (2 :: Int) $ \y ->
      sIf (sReadVar y @>@ x) (res @=@ x) (res @= toVal "Ochen zhal")

f4 :: MonadJS m s => m Val -> m Val
f4 =
  sFun1 $ \x res ->
    sWithVar (0 :: Int) $ \l ->
      sWithVar (0 :: Int) $ \r ->
        r @=@ x @#
        sWithVar
          (1 :: Int)
          (\one ->
             sWhile
               (x @>@ (sReadVar l @+@ sReadVar l))
               (l @=@ (sReadVar l @+@ sReadVar one)) @#
             (res @=@ sReadVar l))
