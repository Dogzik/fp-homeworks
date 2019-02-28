module Task2
    ( Neg
    , doubleNeg
    , excludedNeg
    , doubleNegElim
    ) where

import Data.Void (Void)
import Data.Function (fix)

type Neg a = a -> Void

doubleNeg :: a -> Neg (Neg a)
doubleNeg = \a f -> f a

excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg = \foo -> (foo . Right) (foo . Left)

-- pierce :: ((a -> b) -> a) -> a
-- pierce = undefined

doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = fix id
-- Рассмотрим оценку ИИВ с помощью открытых множеств и какого-то
-- топологического простанства. Пусть это простанство над R. Рассмотрим A = (0, 1)
-- [A|!A] = (0, 1) | In(c(0, 1)) = (0, 1) v (-inf, 0) v (1, +inf) =
-- (inf, 0) v (0, 1) v (1, inf) != R - высказывание не общезначимо

--
-- thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
-- thirdNegElim = undefined
