{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}

module Task1
  ( ST
  , newSTRef
  , readSTRef
  , runST
  , writeSTRef
  ) where

import           Control.Monad.State (State, evalState, get, gets, modify)
import           Data.Map.Strict     (Map, empty, insert, (!))
import           Data.Maybe          (fromJust)
import           Data.Typeable       (Typeable, cast)

data AnyValue =
  forall a. Typeable a =>
            AnyValue a

newtype STRef s a =
  STRef Integer
  deriving (Eq, Ord)

data AnySTRef s =
  forall a. AnySTRef (STRef s a)

instance Eq (AnySTRef s) where
  AnySTRef (STRef a) == AnySTRef (STRef b) = a == b

instance Ord (AnySTRef s) where
  AnySTRef (STRef a) `compare` AnySTRef (STRef b) = a `compare` b

data STEnv s =
  STEnv
    { variables :: Map (AnySTRef s) AnyValue
    , maxNum    :: Integer
    }

newtype ST s a =
  ST (State (STEnv s) a)

instance Monad (ST s) where
  return = ST . return
  (ST m) >>= f = ST $ m >>= g
    where
      unpack (ST x) = x
      g = unpack . f

instance Applicative (ST s) where
  pure = return
  ST f <*> ST x = ST $ f <*> x

instance Functor (ST s) where
  fmap f (ST x) = ST $ fmap f x

getEnv :: ST s (STEnv s)
getEnv = ST get

updMaxNum :: Integer -> ST s ()
updMaxNum x = ST $ modify $ \env -> env {maxNum = x}

insertElem :: AnySTRef s -> AnyValue -> ST s ()
insertElem key value =
  ST $ do
    elems <- gets variables
    let newElems = insert key value elems
    modify $ \env -> env {variables = newElems}

newSTRef :: Typeable a => a -> ST s (STRef s a)
newSTRef x = do
  env <- getEnv
  let newNum = maxNum env + 1
  updMaxNum newNum
  let newRef = STRef newNum
  insertElem (AnySTRef newRef) (AnyValue x)
  return newRef

writeSTRef :: Typeable a => STRef s a -> a -> ST s ()
writeSTRef ref x = insertElem (AnySTRef ref) (AnyValue x)

getValue :: Typeable a => AnyValue -> a
getValue (AnyValue x) = fromJust $ cast x

readSTRef :: Typeable a => STRef s a -> ST s a
readSTRef ref = do
  env <- getEnv
  let anyVal = (variables env) ! (AnySTRef ref)
  return $ getValue anyVal

runST :: (forall s. ST s a) -> a
runST (ST evaluation) = evalState evaluation $ STEnv empty 0
