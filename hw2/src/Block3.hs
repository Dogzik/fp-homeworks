module Block3
  ( Parser (..)
  , element
  , eof
  , ok
  , parseCBS
  , parseInteger
  , parseIntegerStrict
  , parseListListInteger
  , parseListListIntegerStrict
  , parser
  , satisfy
  , stream
  ) where

import Control.Applicative (Alternative, empty, some, (<|>), many)
import Data.Char (isDigit, isSpace)
import Control.Monad (replicateM)
import Text.Read (readMaybe)

-- task1
data Parser s a = Parser { runParser :: ([s] -> Maybe (a, [s])) }

parser :: ([s] -> Maybe (a, [s])) -> Parser s a
parser p = Parser { runParser = p }

instance Functor (Parser s) where
  fmap f p = parser $ \input -> do
    (a, rest) <- runParser p input
    return (f a, rest)

instance Applicative (Parser s) where
  pure a = parser $ \input -> return (a, input)

  pAB <*> pA = parser $ \input -> do
    (ab, s1) <- runParser pAB input
    (a, s2) <- runParser pA s1
    return (ab a, s2)

instance Monad (Parser s) where
  return = pure

  pA >>= f = parser $ \input -> do
    (a, s1) <- runParser pA input
    runParser (f a) s1

instance Alternative (Parser s) where
  empty = parser $ const Nothing

  f <|> g = parser $ \input -> runParser f input <|> runParser g input

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

parseIntegerText :: Parser Char [Char]
parseIntegerText = ((flip const <$> element '+') <|> ((:) <$> element '-') <|> (flip const <$> ok)) <*> (some $ satisfy isDigit)
    
parseInteger :: Parser Char Integer
parseInteger = read <$> parseIntegerText

parseIntegerStrict :: Parser Char Integer
parseIntegerStrict = parseInteger <* eof
  
-- task4
parseSpace :: Parser Char ()
parseSpace = const () <$> (many $ satisfy isSpace)

parseListListInteger :: Parser Char [[Integer]]
parseListListInteger = ((:) <$> parseOneList <*> (many $ parseSpace *> element ',' *> parseOneList)) <|> (const [] <$> ok)
  where
    safeReadInt s = case readMaybe s of 
                      Nothing -> empty
                      Just x  -> return x
    parseOneInt = (parseSpace *> parseIntegerText) >>= safeReadInt
    parseOneInteger = parseSpace *> element ',' *> parseSpace *> parseInteger
    parseOneList = parseOneInt >>= (flip replicateM parseOneInteger)

parseListListIntegerStrict :: Parser Char [[Integer]]
parseListListIntegerStrict = parseListListInteger <* parseSpace <* eof