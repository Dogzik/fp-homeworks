module Parser
  ( parseDollarExpr
  , parseIdentifier
  , parseSingleQuote
  ) where

import Control.Applicative (liftA2)
import Data.Void (Void)
import Structure (DollarExpr (..), Program)
import Text.Megaparsec (ParseErrorBundle, Parsec, anySingleBut, many, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, letterChar)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

type ParserError = ParseErrorBundle String Void

parseIdentifier :: Parser String
parseIdentifier =
  liftA2 (:) (letterChar <|> underscope) (many (alphaNumChar <|> underscope))
  where
    underscope = char '_'

parseSubshell :: Parser Program
parseSubshell = char '(' *> parseProgram <* char ')'

parseProgram :: Parser Program
parseProgram = undefined

parseDollarExpr :: Parser DollarExpr
parseDollarExpr =
  char '$' *> (smallPosArg <|> bigPosArg <|> envVar <|> inlineCall)
  where
    smallPosArg = PosArg . read . pure <$> digitChar
    bigPosArg = PosArg <$> (char '{' *> decimal <* char '{')
    envVar = EnvVar <$> parseIdentifier
    inlineCall = InlineCall <$> parseSubshell

parseSingleQuote :: Parser String
parseSingleQuote = singleQuote *> body <* singleQuote
  where
    singleQuote = char '\''
    body = liftA2 (:) (anySingleBut '\'') body <|> pure ""
