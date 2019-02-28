{-# LANGUAGE TypeOperators #-}

module Task1
    ( associator
    , distributivity
    , eitherAssoc
    ) where

distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity  (Left a)       = (Left a, Left a)
distributivity  (Right (b, c)) = (Right b, Right c)

associator :: (a, (b, c)) -> ((a, b), c)
associator (a, (b, c)) = ((a, b), c)

type (<->) a b = (a -> b, b -> a)

eitherAssocLeft :: Either a (Either b c) -> Either (Either a b) c
eitherAssocLeft (Left a)          = Left (Left a)
eitherAssocLeft (Right (Left b))  = Left (Right b)
eitherAssocLeft (Right (Right c)) = Right c

eitherAssocRight :: Either (Either a b) c -> Either a (Either b c)
eitherAssocRight (Left (Left a))  = Left a
eitherAssocRight (Left (Right b)) = Right (Left b)
eitherAssocRight (Right c)        = Right (Right c)

eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc = (eitherAssocLeft, eitherAssocRight)
