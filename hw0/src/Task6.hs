module Task6
    ( e1
    , e2
    ) where

import Data.Maybe (mapMaybe)
import Task1 (distributivity)

e1 :: (Either String b, Either String c)
e1 = distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))
-- WHNF e1 = (Left ("harold" ++ " hide " ++ "the " ++ "pain"), Left ("harold" ++ " hide " ++ "the " ++ "pain"))

foo :: Char -> Maybe Double
foo char =
  case char == 'o' of
    True  -> Just $ exp pi
    False -> Nothing

e2 :: Bool
e2 = null $ mapMaybe foo "pole chudes ochen' chudesno"
-- WHNF e2 = False
-- Но это произовйдёт когда мы в состоянии (exp pi):(mapMaybe foo "le chudes ochen' chudesno")
