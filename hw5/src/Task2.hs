{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

module Task2
  ( MonadJS(..)
  , Number(..)
  , Val(..)
  , VarJS(..)
  , ToVal(..)
  , runSTJS
  , runSTJS1
  , f1
  , f2
  , f3
  , f4
  , f5
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
  | Undefined
  deriving (Show)

addJS :: Val -> Val -> Val
addJS Undefined _ = Undefined
addJS (Boolean a) (Boolean b) = Boolean $ a || b
addJS (Boolean a) (Numb b) = Numb $ b + Inter (fromEnum a)
addJS (Boolean a) (Str b) = Str $ show a ++ b
addJS (Numb a) (Numb b) = Numb $ a + b
addJS (Numb a) (Str b) =
  case a of
    Inter x -> Str $ show x ++ b
    Frac y  -> Str $ show y ++ b
addJS (Str a) (Str b) = Str $ a <> b
addJS a b = addJS b a

subJS :: Val -> Val -> Val
subJS Undefined _             = Undefined
subJS _ Undefined             = Undefined
subJS (Boolean a) (Boolean b) = Boolean $ (a || b) && (not a || not b) --xor
subJS (Str _) (Str _)         = Str mempty
subJS (Numb a) (Numb b)       = Numb $ a - b
subJS _ _                     = Str mempty

mulJS :: Val -> Val -> Val
mulJS Undefined _ = Undefined
mulJS (Boolean a) (Boolean b) = Boolean $ a && b
mulJS (Boolean a) b@(Numb _) =
  if a
    then b
    else Numb $ Inter 0
mulJS (Boolean a) b@(Str _) =
  if a
    then b
    else Str mempty
mulJS (Numb a) (Numb b) = Numb $ a * b
mulJS (Numb a) (Str b) =
  case a of
    Inter x -> Str $ concatMap (const b) $ replicate x []
    Frac y  -> Str $ concatMap (const b) $ replicate (floor y) []
mulJS (Str a) (Str b) = Str $ concat $ map (:) a <*> map pure b
mulJS a b = mulJS b a

divJS :: Val -> Val -> Val
divJS Undefined _ = Undefined
divJS _ Undefined = Undefined
divJS (Boolean a) (Boolean b) = Boolean $ not a && b
divJS (Numb a) (Numb b) =
  case a of
    Inter x ->
      case b of
        Inter y -> Numb $ Inter $ x `div` y
        Frac z  -> Numb $ Inter $ floor $ fromIntegral x / z
    Frac x ->
      case b of
        Inter y -> Numb $ Inter $ floor $ x / fromIntegral y
        Frac z  -> Numb $ Inter $ floor $ x / z
divJS _ _ = Undefined

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
  infixl 7 @>@
  (@>@) :: m Val -> m Val -> m Bool
  infixl 5 @#
  (@#) :: m () -> m a -> m a
  infixl 8 @+@
  (@+@) :: m Val -> m Val -> m Val
  infixl 8 @-@
  (@-@) :: m Val -> m Val -> m Val
  infixl 9 @*@
  (@*@) :: m Val -> m Val -> m Val
  infixr 9 @/@
  (@/@) :: m Val -> m Val -> m Val
  infix 6 @=@
  (@=@) :: m (Var m Val) -> m Val -> m ()
  sReadVar :: m (Var m Val) -> m Val
  sIf :: m Bool -> m () -> m () -> m ()
  sFun1 :: (m Val -> m (Var m Val) -> m ()) -> m Val -> m Val
  sWhile :: m Bool -> m () -> m ()

class VarJS a where
  sWithVar :: MonadJS m s => a -> (m (Var m Val) -> m ()) -> m ()
  infix 6 @=
  (@=) :: MonadJS m s => m (Var m Val) -> a -> m ()

instance MonadJS (ST s) s where
  type Var (ST s) a = STRef s a
  newVar = newSTRef
  writeVar = writeSTRef
  (@>@) = liftA2 greater
  (@#) = (>>)
  (@+@) = liftA2 addJS
  (@-@) = liftA2 subJS
  (@*@) = liftA2 mulJS
  (@/@) = liftA2 divJS
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
    resRef <- newSTRef Undefined
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
               (x @>@ sReadVar l @+@ sReadVar l)
               (l @=@ sReadVar l @+@ sReadVar one) @#
             (res @=@ sReadVar l))

f5 :: MonadJS m s => m Val -> m Val
f5 =
  sFun1 $ \x res ->
    sWithVar "Wow" $ \y ->
      y @=@ x @+@ return (toVal (20 :: Int)) @# res @=@ f4 (sReadVar y)
