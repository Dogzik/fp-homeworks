module Block3
  ( Building(..)
  , Castle(..)
  , City(..)
  , FamilyError(..)
  , House(..)
  , Human(..)
  , Lord(..)
  , LordError(..)
  , Nat(..)
  , Tree(..)
  , Walls(..)
  , WallsError(..)
  , acceptFamily
  , acceptLord
  , afterDays
  , buildBuiding
  , buildCastle
  , buildWalls
  , contains
  , daysToParty
  , divNat
  , emplace
  , empty
  , erase
  , fromList
  , isEvenNat
  , isWeekend
  , modNat
  , nextDay
  , size
  ) where

import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.Monoid (Sum (..), getSum)

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
      _ -> error "Incorect input"

instance Bounded Day where
  maxBound = Sunday
  minBound = Monday

daysInWeek :: Int
daysInWeek = 1 + fromEnum (maxBound :: Day)

nextDay :: Day -> Day
nextDay Sunday = Monday
nextDay day    = succ day

afterDays :: Day -> Int -> Day
afterDays day cnt = toEnum $ (cnt + fromEnum day) `mod` daysInWeek

isWeekend :: Day -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

daysToParty :: Day -> Int
daysToParty day = fromEnum $ (fromEnum Friday - fromEnum day) `mod` daysInWeek

-- task 2
data Walls = Walls
data Lord = Lord
data Human = Human
data House
  = House1 Human
  | House2 Human Human
  | House3 Human Human Human
  | House4 Human Human Human Human
data Castle = Castle (Maybe Walls) (Maybe Lord)
data Building = Church | Library
data City = City
  { castle   :: Maybe Castle
  , building :: Maybe Building
  , houses   :: NonEmpty House
}
data FamilyError = TooSmall | TooBig
data LordError = NoCastle | AlreadyWasLord
data WallsError = NeedCastle | NoLord | TooFewPeople | AlreadyWereWalls

buildCastle :: City -> (City, Bool)
buildCastle city =
  case castle city of
    Just _  -> (city, False)
    Nothing -> (city { castle = Just (Castle Nothing Nothing) }, True)

buildBuiding :: City -> Building -> (City, Bool)
buildBuiding city b =
  case building city of
    Just _  -> (city, False)
    Nothing -> (city { building = Just b }, True)

acceptFamily :: City -> [Human] -> (City, Maybe FamilyError)
acceptFamily city@City { houses = x :| xs} family =
  case family of
    []           -> (city, Just TooSmall)
    [a]          -> (city { houses = House1 a :| (x:xs) }, Nothing)
    [a, b]       -> (city { houses = House2 a b :| (x:xs) }, Nothing)
    [a, b, c]    -> (city { houses = House3 a b c :| (x:xs) }, Nothing)
    [a, b, c, d] -> (city { houses = House4 a b c d :| (x:xs) }, Nothing)
    _            -> (city, Just TooBig)

acceptLord :: City -> (City, Maybe LordError)
acceptLord city =
  case castle city of
    Nothing                     -> (city, Just NoCastle)
    Just (Castle _ (Just Lord)) -> (city, Just AlreadyWasLord)
    Just (Castle walls Nothing) -> (city { castle = Just (Castle walls (Just Lord)) }, Nothing)

peopleCnt :: NonEmpty House -> Int
peopleCnt xs = getSum $ foldMap mapper xs
  where
    mapper (House1 _)       = Sum 1
    mapper (House2 _ _)     = Sum 2
    mapper (House3 _ _ _)   = Sum 3
    mapper (House4 _ _ _ _) = Sum 4

buildWalls :: City -> (City, Maybe WallsError)
buildWalls city =
  case castle city of
    Nothing                           -> (city, Just NeedCastle)
    Just (Castle (Just Walls) _)      -> (city, Just AlreadyWereWalls)
    Just (Castle Nothing Nothing)     -> (city, Just NoLord)
    Just (Castle Nothing (Just Lord)) -> checkPeople city
  where
    checkPeople checkedCity =
      if peopleCnt (houses checkedCity) >= 10
      then (city {castle = Just (Castle (Just Walls) (Just Lord)) }, Nothing)
      else (city, Just TooFewPeople)

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

empty :: Tree a -> Bool
empty Leaf = True
empty _    = False

size :: Tree a -> Int
size Leaf                    = 0
size (Node elems left right) = length elems + size left + size right

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
