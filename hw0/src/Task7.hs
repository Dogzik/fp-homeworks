{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Task7
    ( expr1
    , expr2
    , expr3
    ) where
import Data.Either (lefts, rights)

expr1 :: Bool
expr1 =
  let t1 = (" Grey" :: [Char]) in
  let t2 = ("Dorian " :: [Char]) in
  let t3 = ((++) :: [Char] -> [Char] -> [Char]) in
  let t4 = (t3 t2 :: [Char] -> [Char]) in
  let t5 = ((t4, t1) :: ([Char] -> [Char], [Char])) in
  let t6 = ([t5] :: [([Char] -> [Char], [Char])]) in
  let t7 = (id :: ([Char] -> [Char]) -> [Char] -> [Char]) in
  let t8 = (uncurry :: (([Char] -> [Char]) -> [Char] -> [Char]) -> ([Char] -> [Char], [Char]) -> [Char]) in
  let t9 = (t8 t7 :: ([Char] -> [Char], [Char]) -> [Char]) in
  let t10 = (map :: (([Char] -> [Char], [Char]) -> [Char]) -> [([Char] -> [Char], [Char])] -> [[Char]]) in
  let t11 = (t10 t9 :: [([Char] -> [Char], [Char])] -> [[Char]]) in
  let t12 = (t11 t6 :: [[Char]]) in
  let t13 = (head :: [[Char]] -> [Char]) in
  let t14 = (null :: [Char] -> Bool) in
  let t15 = ((.) :: ([Char] -> Bool) -> ([[Char]] -> [Char]) -> [[Char]] -> Bool) in
  let t16 = (t14 `t15` t13 :: [[Char]] -> Bool) in
  let t17 = (($) :: ([[Char]] -> Bool) -> [[Char]] -> Bool) in
  t16 `t17` t12

expr2_1 :: Num t => t
expr2_1 =
  let t1 = (1 :: Num t => t) in
  let t2 = (2 :: Num t => t) in
  let t3 = ((+) :: Num t => t -> t -> t)
  in t1 `t3` t2

expr2_2 :: Num t => t
expr2_2 =
  let t1 = (2 :: Num t => t) in
  let t2 = (6 :: Integral t => t) in
  let t3 = ((^) :: (Num t, Integral s) => t -> s -> t) in
  t1 `t3` t2

expr2_3 :: forall t s. (Num t, Num s) => [Either t s]
expr2_3 =
  let t1 = (Left expr2_1 :: Either t s) in
  let t2 = (Right expr2_2 :: Either t s) in
  [t1, t2]

expr2_4 :: forall t s. (Num t, Num s) => [Either t s] -> [(t, s)]
expr2_4 x =
  let t1 = (lefts :: [Either t s] -> [t]) in
  let t2 = (rights :: [Either t s] -> [s]) in
  let t3 = (zip :: [t] -> [s] -> [(t, s)]) in
  let t4 = (t1 x :: [t]) in
  let t5 = (t2 x :: [s]) in
  let t6 = (t3 t4 :: [s] -> [(t, s)]) in
  t6 t5

expr2 :: (Num t, Num s) => [(t, s)]
expr2 = expr2_4 expr2_3

expr3 :: forall t. (Integral t, Eq t) => t -> Bool
expr3 =
  let t1 = (not :: Bool -> Bool) in
  let t2 = ((||) :: Bool -> Bool -> Bool) in
  let impl = ((\x y -> (t1 x :: Bool) `t2` y) :: Bool -> Bool -> Bool) in
  let t3 = (mod :: t -> t -> t) in
  let t4 = (2 :: t) in
  let t5 = (4 :: t) in
  let t6 = (0 :: t) in
  let t7 = ((==) :: t -> t -> Bool) in
  let isMod2 = ((\x -> ((x `t3` t4) :: t) `t7` t6) :: t -> Bool) in
  let isMod4 = ((\x -> ((x `t3` t5) :: t) `t7` t6) :: t -> Bool) in
  \x -> ((isMod2 x) :: Bool) `impl` ((isMod4 x) :: Bool)
