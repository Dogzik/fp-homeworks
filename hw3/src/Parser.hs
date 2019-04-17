module Parser
  ( parseDollarExpr
  , parseDoubleQuote
  , parseIdentifier
  , parseSingleQuote
  , parseArg
  ) where

import Control.Applicative (liftA2)
import Control.Monad (void)
import Data.Void (Void)
import Structure (Arg, ArgFragment (..), DollarExpr (..), Program)
import Text.Megaparsec (ParseErrorBundle, Parsec, anySingle, anySingleBut, eof, lookAhead, many,
                        noneOf, notFollowedBy, some, try, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, letterChar, spaceChar)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

type ParserError = ParseErrorBundle String Void

parseIdentifier :: Parser String
parseIdentifier =
  liftA2
    (:)
    (try letterChar <|> try underscope)
    (many (try alphaNumChar <|> try underscope))
  where
    underscope = char '_'

parseSubshell :: Parser Program
parseSubshell = char '(' *> parseProgram <* char ')'

parseProgram :: Parser Program
parseProgram = undefined

parseDollarExpr :: Parser DollarExpr
parseDollarExpr =
  char '$' *>
  (try smallPosArg <|> try bigPosArg <|> try envVar <|> try inlineCall)
  where
    smallPosArg = PosArg . read . pure <$> digitChar
    bigPosArg = PosArg <$> (char '{' *> decimal <* char '}')
    envVar = EnvVar <$> parseIdentifier
    inlineCall = InlineCall <$> parseSubshell

parseSingleQuote :: Parser String
parseSingleQuote = singleQuote *> body <* singleQuote
  where
    singleQuote = char '\''
    body = many (anySingleBut '\'')

parseEscaped :: Parser Char -> Parser Char
parseEscaped p = char '\\' *> p

parseEscapedChar :: Char -> Parser Char
parseEscapedChar c = parseEscaped $ char c

parseDoubleQuote :: Parser [ArgFragment]
parseDoubleQuote = doubleQuote *> body <* doubleQuote
  where
    doubleQuote = char '\"'
    escaped =
      Symbol <$>
      (try (parseEscapedChar '\\') <|> try (parseEscapedChar '\"') <|>
       try (parseEscapedChar '$'))
    dollarExpr = Expr <$> parseDollarExpr
    anyButQuote = Symbol <$> anySingleBut '\"'
    body =
      liftA2 (:) (try escaped <|> try dollarExpr <|> try anyButQuote) body <|>
      pure []

parseArg :: Parser Arg
parseArg =
  fragments <*
  (try eof <|> try (void $ lookAhead spaceChar) <|>
   try (void (lookAhead $ char ';')))
  where
    escaped = pure . Symbol <$> parseEscaped anySingle
    singleQuote = pure . Symbols <$> parseSingleQuote
    doubleQuote = parseDoubleQuote
    dollarExpr = pure . Expr <$> parseDollarExpr
    simpleChar =
      pure . Symbol <$> (notFollowedBy spaceChar *> noneOf ['(', ')', ';'])
    fragments =
      concat <$>
      some
        (try escaped <|> try singleQuote <|> try doubleQuote <|> try dollarExpr <|>
         try simpleChar)
