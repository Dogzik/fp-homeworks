module Block3
  ( nextDay
  , afterDays
  , isWeekend
  , daysToParty
  , Nat(..)
  , isEvenNat
  , divNat
  , modNat
  , Tree(..)
  , empty
  , size
  , contains
  , emplace
  , fromList
  , erase
  ) where

import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.Maybe ()

-- task 1
data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show)

instance Enum Day where
  fromEnum day =
    case day of
      Monday    -> 0
      Tuesday   -> 1
      Wednesday -> 2
      Thursday  -> 3
      Friday    -> 4
      Saturday  -> 5
      Sunday    -> 6

  toEnum x =
    case x of
      0 -> Monday
      1 -> Tuesday
      2 -> Wednesday
      3 -> Thursday
      4 -> Friday
      5 -> Saturday
      6 -> Sunday
      _ -> error "Incorecnt input"

nextDay :: Day -> Day
nextDay Sunday = Monday
nextDay day    = succ day

afterDays :: Day -> Int -> Day
afterDays day cnt = toEnum $ (cnt + fromEnum day) `mod` (1 + fromEnum Sunday)

isWeekend :: Day -> Bool
isWeekend day =
  case day of
    Saturday -> True
    Sunday   -> True
    _        -> False

daysToParty :: Day -> Int
daysToParty day = fromEnum $ (fromEnum Friday - fromEnum day) `mod` (1 + fromEnum Sunday)

-- task 2



-- task 3
data Nat
  = Z
  | S Nat
  deriving (Show)

instance Num Nat where
  a     + Z     = a
  a     + (S b) = S (a + b)
  _     * Z     = Z
  a     * (S b) = a + (a * b)
  a     - Z     = a
  Z     - _     = Z
  (S a) - (S b) = a - b
  fromInteger 0 = Z
  fromInteger x = S (fromInteger (x - 1))
  abs = id
  signum Z = 0
  signum _ = 1

instance Eq Nat where
  Z     == Z     = True
  (S a) == (S b) = a == b
  _     == _     = False

instance Ord Nat where
  Z     <= _     = True
  (S _) <= Z     = False
  (S a) <= (S b) = a <= b

isEvenNat :: Nat -> Bool
isEvenNat Z     = True
isEvenNat (S a) = not $ isEvenNat a

divNat :: Nat -> Nat -> Nat
divNat _ Z = error "Dividing by Z"
divNat a b =
  if a >= b
  then S (divNat (a - b) b)
  else Z

modNat :: Nat -> Nat -> Nat
modNat a b = a - divNat a b * b

-- task 4
data Tree a
  = Leaf
  | Node (NonEmpty a) (Tree a) (Tree a)
  deriving (Show)

empty :: Ord a => Tree a -> Bool
empty Leaf = True
empty _    = False

size :: Ord a => Tree a -> Int
size Leaf                = 0
size (Node _ left right) = 1 + size left + size right

contains :: Ord a => a -> Tree a -> Maybe (Tree a)
contains _ Leaf = Nothing
contains x node@(Node (e :| _) left right) =
  case compare x e of
    EQ -> Just node
    LT -> contains x left
    GT -> contains x right

emplace :: Ord a => a -> Tree a -> Tree a
emplace x Leaf = Node (x :| []) Leaf Leaf
emplace x (Node elems@(e :| _) left right) =
  case compare x e of
    EQ -> Node (x <| elems) left right
    LT -> Node elems (emplace x left) right
    GT -> Node elems left (emplace x right)

fromList :: Ord a => [a] -> Tree a
fromList = foldl' (flip emplace) Leaf

erase :: Ord a => a -> Tree a -> Tree a
erase _ tree@Leaf = tree
erase x (Node elems@(e :| es) left right) =
  case compare x e of
    LT -> Node elems (erase x left) right
    GT -> Node elems left (erase x right)
    EQ -> case es of
            h:t -> Node (h :| t) left right
            [] -> case extractLeftestNode right of
                    Nothing                   -> left
                    Just (newElems, newRight) -> Node newElems left newRight
  where
    extractLeftestNode Leaf = Nothing
    extractLeftestNode (Node smth l r) =
      case extractLeftestNode l of
        Nothing                    -> Just (smth, r)
        Just (childElems, newLeft) -> Just (childElems, Node smth newLeft r)
