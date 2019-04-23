module Parser
  ( parseProgram
  ) where

import Control.Applicative (liftA2)
import Control.Monad (void)
import Data.Void (Void)
import Structure (Arg, ArgFragment (..), Assignment (..), Command (..), DQFragment (..),
                  DollarExpr (..), ElifClause (..), ElseClause (..), IfClause (..), Program,
                  SingleCommand (..), WhileClause (..))
import Text.Megaparsec (ParseErrorBundle, Parsec, anySingle, anySingleBut, eof, many, noneOf,
                        notFollowedBy, some, try, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, letterChar, newline, space, spaceChar,
                             string)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

type ParserError = ParseErrorBundle String Void

manyBacktrace :: Parser a -> Parser [a]
manyBacktrace = many . try

parseIdentifier :: Parser String
parseIdentifier =
  liftA2
    (:)
    (try letterChar <|> try underscope)
    (manyBacktrace (try alphaNumChar <|> try underscope))
  where
    underscope = char '_'

parseSubshell :: Parser Program
parseSubshell =
  char '(' *> space *> (try nonEmptyBody <|> try ([] <$ space)) <* char ')'
  where
    optionalEnd = try commandSuffix <|> try (void $ manyBacktrace safeSpace)
    nonEmptyBody =
      (:) <$> parseCommandWithoutDelimiter <*>
      manyBacktrace (try (commandSuffix *> parseCommandWithoutDelimiter)) <*
      optionalEnd

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
    body = manyBacktrace (anySingleBut '\'')

parseEscaped :: Parser Char -> Parser Char
parseEscaped p = char '\\' *> p

parseEscapedChar :: Char -> Parser Char
parseEscapedChar c = parseEscaped $ char c

parseDoubleQuote :: Parser [DQFragment]
parseDoubleQuote = doubleQuote *> body <* doubleQuote
  where
    doubleQuote = char '\"'
    tryEscapedChar = try . parseEscapedChar
    escaped =
      JustSymbol <$>
      (tryEscapedChar '\\' <|> tryEscapedChar '\"' <|> tryEscapedChar '$')
    dollarExpr = Subst <$> parseDollarExpr
    anyButQuote = JustSymbol <$> anySingleBut '\"'
    body = manyBacktrace (try escaped <|> try dollarExpr <|> try anyButQuote)

parseArg :: Parser Arg
parseArg =
  some
    (try escaped <|> try singleQuote <|> try doubleQuote <|> try dollarExpr <|>
     try simpleChar)
  where
    escaped = Symbol <$> parseEscaped anySingle
    singleQuote = SingleQuotes <$> parseSingleQuote
    doubleQuote = DoubleQuotes <$> parseDoubleQuote
    dollarExpr = Expr <$> parseDollarExpr
    simpleChar =
      Symbol <$>
      (notFollowedBy eof *> notFollowedBy spaceChar *> noneOf ['(', ')', ';'])

safeSpace :: Parser Char
safeSpace = notFollowedBy newline *> spaceChar

commandDelimiter :: Parser ()
commandDelimiter = try eof <|> void (try newline) <|> void (try $ char ';')

commandSuffix :: Parser ()
commandSuffix = manyBacktrace safeSpace *> commandDelimiter *> space

parseAssignment :: Parser Assignment
parseAssignment = Assignment <$> parseIdentifier <* char '=' <*> parseArg

parseSingleCommand :: Parser SingleCommand
parseSingleCommand = SingleCommand <$> parsePart <*> manyBacktrace parsePart
  where
    parsePart = parseArg <* manyBacktrace safeSpace

commandButKeywords :: [String] -> Parser Command
commandButKeywords keywords = checkKeywords keywords *> parseCommand
  where
    checkKeywords = foldr ((*>) . notFollowedBy . string) (pure ())

someCommands :: [String] -> Parser [Command]
someCommands keywords = some $ commandButKeywords keywords

manyCommands :: [String] -> Parser [Command]
manyCommands keywords = manyBacktrace $ commandButKeywords keywords

parseWhileClause :: Parser WhileClause
parseWhileClause =
  WhileClause <$> (string "while" *> space *> someCommands ["do"]) <*
  string "do" <*
  space <*>
  manyCommands ["done"] <*
  string "done"

parseElseClause :: Parser ElseClause
parseElseClause = ElseClause <$> (string "else" *> space *> manyCommands ["fi"])

parseElifClause :: Parser ElifClause
parseElifClause =
  ElifClause <$> (string "elif" *> space *> someCommands ["then"]) <*
  string "then" <*
  space <*>
  manyCommands ["elif", "else", "fi"]

parseIfClause :: Parser IfClause
parseIfClause =
  IfClause <$> (string "if" *> space *> someCommands ["then"]) <* string "then" <*
  space <*>
  manyCommands ["elif", "else", "fi"] <*>
  manyBacktrace parseElifClause <*>
  maybeParse parseElseClause <*
  string "fi"
  where
    maybeParse p = (Just <$> try p) <|> pure Nothing

parseCommandWithoutDelimiter :: Parser Command
parseCommandWithoutDelimiter =
  try assignCommand <|> try whileCommand <|> try ifCommand <|> try subshell <|>
  try simpleCommand
  where
    assignCommand = AssignCommand <$> parseAssignment
    ifCommand = If <$> parseIfClause
    whileCommand = While <$> parseWhileClause
    simpleCommand = SimpleCommand <$> parseSingleCommand
    subshell = Subshell <$> parseSubshell

parseCommand :: Parser Command
parseCommand = parseCommandWithoutDelimiter <* commandSuffix

parseProgram :: Parser Program
parseProgram = space *> manyBacktrace parseCommand <* eof
