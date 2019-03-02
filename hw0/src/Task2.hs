module Task2
    ( Neg
    , doubleNeg
    , excludedNeg
    , doubleNegElim
    , pierce
    , thirdNegElim
    ) where

import Data.Void (Void)
import Data.Function (fix)

type Neg a = a -> Void

doubleNeg :: a -> Neg (Neg a)
doubleNeg = \a f -> f a

excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg = \foo -> (foo . Right) (foo . Left)

pierce :: ((a -> b) -> a) -> a
pierce = fix id
-- Рассмотрим оценку ИИВ с помощью открытых множеств и какого-то
-- топологического простанства. Пусть это простанство над R
-- a = (-inf, 228) v (228, inf)
-- b = {} - пустое множество
-- a -> b = In(c(a) + b) = In({228} v {}) = {}
-- (a -> b) -> a = R
-- ((a -> b) -> a) -> a = {} v ((-inf 228) v (228, inf)) != R
-- Выысказывание не общезначимо

doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = fix id
-- Рассмотрим оценку ИИВ с помощью открытых множеств и какого-то
-- топологического простанства. Пусть это простанство над R. Рассмотрим A = (0, 1)
-- [A|!A] = (322, 1488) | In(c(322, 1488)) = (322, 1488) v (-inf, 322) v (1488, +inf) =
-- (inf, 322) v (322, 1488) v (1488, inf) != R - высказывание не общезначимо

controposition :: (a -> b) -> (Neg b -> Neg a)
controposition = \f g -> g . f

thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim = controposition doubleNeg
