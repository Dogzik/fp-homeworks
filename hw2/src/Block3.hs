module Block3
  ( Parser (..)
  , element
  , eof
  , ok
  , parseCBS
  , parser
  , satisfy
  , stream
  ) where

import Control.Applicative (Alternative, empty, (<|>))

-- task1
newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

parser :: ([s] -> Maybe (a, [s])) -> Parser s a
parser p = Parser { runParser = p }

instance Functor (Parser s) where
  fmap f (Parser p)  = parser $ \input -> do
    (a, rest) <- p input
    return (f a, rest)

instance Applicative (Parser s) where
  pure a = Parser $ \input -> return (a, input)

  Parser pAB <*> Parser pA = parser $ \input -> do
    (ab, s1) <- pAB input
    (a, s2) <- pA s1
    return (ab a, s2)

instance Monad (Parser s) where
  return = pure

  Parser pA >>= f = parser $ \input -> do
    (a, s1) <- pA input
    let pB = f a
    runParser pB s1

instance Alternative (Parser s) where
  empty = parser $ const Nothing

  Parser f <|> Parser g = parser $ \input -> f input <|> g input

-- task2
ok :: Parser s ()
ok = parser $ \input -> return ((), input)

eof :: Parser s ()
eof = parser $ \input -> if null input
                         then return ((), input)
                         else Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy p = parser $ \input -> case input of
                                 []   -> Nothing
                                 x:xs -> if p x
                                         then return (x, xs)
                                         else Nothing

element :: Eq s => s -> Parser s s
element e = satisfy (== e)

stream :: Eq s => [s] -> Parser s [s]
stream es = traverse element es

-- task3
parseCBS :: Parser Char ()
parseCBS = s <* eof
  where
    s = (element '(' *> s *> element ')' *> s) <|> ok

    