module Block2
  ( ArithmeticError (..)
  , Expr (..)
  , eval
  , moving
  ) where

import Control.Monad.State.Lazy (State, execState, get, modify)
import Numeric.Natural (Natural)

-- task1
data Expr
  = Const Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr

data ArithmeticError
   = DivisionByZero
   | NegativeExponent
   deriving (Show, Eq)

biEval :: (Int -> Int -> Either ArithmeticError Int) -> Expr -> Expr -> Either ArithmeticError Int
biEval op l r = do
  lRes <- eval l
  rRes <- eval r
  op lRes rRes

safeOp :: (Int -> Int -> Int) -> Int -> Int -> Either ArithmeticError Int
safeOp op l r = return $ op l r

eval :: Expr -> Either ArithmeticError Int
eval (Const x) = return x
eval (Add l r) = biEval (safeOp (+)) l r
eval (Sub l r) = biEval (safeOp (-)) l r
eval (Mul l r) = biEval (safeOp (*)) l r
eval (Div l r) = biEval (\a b -> if b == 0 then Left DivisionByZero else return $ a `div` b) l r
eval (Pow l r) = biEval (\a b -> if b < 0 then Left NegativeExponent else return $ a ^ b) l r

-- task2

data MoveState a = MoveState
  { msHead    :: [a]
  , msTail    :: [a]
  , msMaxSize :: Natural
  , msSize    :: Natural
  , msSum     :: a
  , msAns     :: [a]
  }

changeSum :: Fractional a => a -> MoveState a -> MoveState a
changeSum delta s = s { msSum = msSum s + delta}

incSize :: Fractional a => MoveState a -> MoveState a
incSize s = s { msSize = msSize s + 1 }

checkSize :: Fractional a => MoveState a -> MoveState a
checkSize s = let curMax = msMaxSize s in
  if msSize s <= curMax
  then s
  else let curHead = msHead s
       in let newState = s { msSize = curMax, msHead = tail curHead }
       in changeSum (negate $ head curHead) newState

addAverege :: Fractional a => MoveState a -> MoveState a
addAverege s@MoveState { msSize = curSize, msSum = curSum, msAns = curAns} =
  s { msAns = (curSum / fromIntegral curSize):curAns}

countOne :: Fractional a => State (MoveState a) Bool
countOne = do
  curState <- get
  case msTail curState of
    [] -> return False
    x:xs -> do
            modify $ changeSum x
            modify incSize
            modify checkSize
            modify $ \s -> s { msTail = xs}
            modify addAverege
            return True

countAll :: Fractional a => State (MoveState a) ()
countAll = do
  changed <- countOne
  if changed
  then countAll
  else return ()

moving :: Fractional a => Natural -> [a] -> [a]
moving sz points =
  let initState = MoveState { msHead = points, msTail = points, msMaxSize = sz, msSize = 0, msSum = 0, msAns = [] }
      revAns = execState countAll initState
  in reverse $ msAns revAns
