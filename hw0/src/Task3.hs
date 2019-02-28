module Task3
    ( composition
    , contraction
    , id
    , s
    , permutation
    ) where

import Prelude hiding (id)

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

composition :: (b -> c) -> (a -> b) -> a -> c
composition = s (const s) const
-- composition f g = s (const f) g

id :: a -> a
id = s const const

contraction :: (a -> a -> b) -> a -> b
contraction = s s (s const)

permutation :: (a -> b -> c) -> b -> a -> c
permutation = s (s (const (s (const s) const)) s) (const const)
