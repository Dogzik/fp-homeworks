{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Interpreter
  ( interpret
  ) where

import Control.Monad (mapM_)
import Control.Monad.Cont (MonadCont, callCC)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState, get, gets, modify, put)
import Control.Monad.Writer (execWriterT)
import Data.Char (isSpace)
import Data.List.Split (wordsBy)
import Data.Map.Strict (insert)
import Enviroment (EnvState (..), MonadConsole (..), PosArgs, getEnvVar, getPosArg)
import Parser (isIdentifier)
import Structure (Arg, ArgFragment (..), Assignment (..), Command (..), DQFragment (..),
                  DollarExpr (..), ElifClause (..), ElseClause (..), IfClause (..), Program,
                  SingleCommand (..), WhileClause (..))
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Text.Read (readMaybe)

data MetaChar
  = One Char
  | Many String
  deriving (Show)

isMetaSpace :: MetaChar -> Bool
isMetaSpace (One c) = isSpace c
isMetaSpace _       = False

interpret ::
     (MonadReader PosArgs m, MonadState EnvState m, MonadConsole m, MonadCont m)
  => Program
  -> (ExitCode -> m ExitCode)
  -> m ExitCode
interpret [] _ = return ExitSuccess
interpret [command] hook = execCommand command hook
interpret (command:rest) hook = do
  _ <- execCommand command hook
  interpret rest hook

execCommand ::
     (MonadReader PosArgs m, MonadState EnvState m, MonadConsole m, MonadCont m)
  => Command
  -> (ExitCode -> m ExitCode)
  -> m ExitCode
execCommand (AssignCommand ass) _ = execAssign ass
execCommand (Subshell sh) _ = execSubShell sh
execCommand (SimpleCommand command) hook = do
  params <- calcSingleCommand command
  case params of
    [] -> return ExitSuccess
    exec:execArgs ->
      case exec of
        "echo" -> execEcho execArgs
        "pwd" -> execPwd execArgs
        "cd" -> execCd execArgs
        "exit" -> execExit execArgs hook
        "read" -> execRead execArgs
        _ -> do
          dir <- gets curDir
          callExternal dir exec execArgs
execCommand (While whileClause) hook =
  execWhile ExitSuccess (whileCond whileClause) (whileBody whileClause) hook
execCommand (If ifClause) hook = do
  code <- interpret (ifCond ifClause) hook
  case code of
    ExitSuccess -> interpret (ifBody ifClause) hook
    _           -> execElifElse (elifClauses ifClause) (elseClause ifClause) hook

execElifElse ::
     (MonadReader PosArgs m, MonadState EnvState m, MonadConsole m, MonadCont m)
  => [ElifClause]
  -> Maybe ElseClause
  -> (ExitCode -> m ExitCode)
  -> m ExitCode
execElifElse [] Nothing _ = return ExitSuccess
execElifElse [] (Just elseCommand) hook = interpret (elseBody elseCommand) hook
execElifElse (cur:rest) maybeElse hook = do
  code <- interpret (elifCond cur) hook
  case code of
    ExitSuccess -> interpret (elifBody cur) hook
    _           -> execElifElse rest maybeElse hook

execWhile ::
     (MonadReader PosArgs m, MonadState EnvState m, MonadConsole m, MonadCont m)
  => ExitCode
  -> Program
  -> Program
  -> (ExitCode -> m ExitCode)
  -> m ExitCode
execWhile oldCode cond body hook = do
  condCode <- interpret cond hook
  case condCode of
    ExitFailure _ -> return oldCode
    ExitSuccess -> do
      newCode <- interpret body hook
      execWhile newCode cond body hook

execEcho ::
     (MonadReader PosArgs m, MonadState EnvState m, MonadConsole m, MonadCont m)
  => [String]
  -> m ExitCode
execEcho [] = writeString "\n"
execEcho (x:xs) =
  if x == "-n"
    then writeString $ unwords xs
    else writeString $ unwords (x : xs) ++ "\n"

execPwd ::
     (MonadReader PosArgs m, MonadState EnvState m, MonadConsole m, MonadCont m)
  => [String]
  -> m ExitCode
execPwd pwdArgs =
  case pwdArgs of
    [] -> do
      dir <- gets curDir
      writeString $ dir ++ "\n"
    _ -> do
      _ <- writeString "Too many arguments for pwd\n"
      return $ ExitFailure 2

execCd ::
     (MonadReader PosArgs m, MonadState EnvState m, MonadConsole m, MonadCont m)
  => [String]
  -> m ExitCode
execCd cdArgs =
  case cdArgs of
    [] -> do
      _ <- writeString "Too few arguments for cd\n"
      return $ ExitFailure 3
    [path] -> do
      dir <- gets curDir
      let newDir = dir </> path
      exists <- directotyExists newDir
      if exists
        then do
          modify (\s -> s {curDir = newDir})
          return ExitSuccess
        else do
          _ <- writeString ("No such directory: " ++ newDir ++ "\n")
          return $ ExitFailure 2
    _ -> do
      _ <- writeString "Too many arguments for cd\n"
      return $ ExitFailure 2

execExit ::
     (MonadReader PosArgs m, MonadState EnvState m, MonadConsole m, MonadCont m)
  => [String]
  -> (ExitCode -> m ExitCode)
  -> m ExitCode
execExit exitArgs hook =
  case exitArgs of
    [] -> do
      _ <- writeString "Too few arguments for exit\n"
      return $ ExitFailure 3
    [arg] ->
      case readMaybe arg of
        Nothing -> do
          _ <- writeString ("Wrong argument for exit: " ++ arg ++ "\n")
          return $ ExitFailure 1
        Just x ->
          if x == 0
            then hook ExitSuccess
            else hook $ ExitFailure x
    _ -> do
      _ <- writeString "Too many arguments for exit\n"
      return $ ExitFailure 2

setEnvVar ::
     (MonadReader PosArgs m, MonadState EnvState m, MonadConsole m, MonadCont m)
  => String
  -> String
  -> m ExitCode
setEnvVar setKey setValue =
  if isIdentifier setKey
    then do
      curEnvVars <- gets envVars
      let newEnvVars = insert setKey setValue curEnvVars
      modify (\s -> s {envVars = newEnvVars})
      return ExitSuccess
    else do
      _ <- writeString (setKey ++ " is not a valid identifier\n")
      return $ ExitFailure 2

execAssign ::
     (MonadReader PosArgs m, MonadState EnvState m, MonadConsole m, MonadCont m)
  => Assignment
  -> m ExitCode
execAssign ass = do
  realArg <- calcArg $ value ass
  case realArg of
    [] -> setEnvVar (key ass) ""
    [s] -> setEnvVar (key ass) s
    _ -> do
      _ <- writeString "Too many argumens for assignment\n"
      return $ ExitFailure 2

doReadAssign ::
     (MonadReader PosArgs m, MonadState EnvState m, MonadConsole m, MonadCont m)
  => [String]
  -> [String]
  -> m ExitCode
doReadAssign [] _ = return ExitSuccess
doReadAssign vars [] = do
  mapM_ (`setEnvVar` "") vars
  return ExitSuccess
doReadAssign [var] values = setEnvVar var $ unwords values
doReadAssign (var:vars) (val:vals) = do
  code <- setEnvVar var val
  case code of
    ExitSuccess   -> doReadAssign vars vals
    ExitFailure x -> return $ ExitFailure x

execRead ::
     (MonadReader PosArgs m, MonadState EnvState m, MonadConsole m, MonadCont m)
  => [String]
  -> m ExitCode
execRead readArgs = do
  (code, input) <- readString
  let strings = words input
  doCode <- doReadAssign readArgs strings
  case code of
    ExitSuccess -> return doCode
    _           -> return code

execSubShell ::
     (MonadReader PosArgs m, MonadState EnvState m, MonadConsole m, MonadCont m)
  => Program
  -> m ExitCode
execSubShell subProgram = do
  curState <- get
  code <- callCC $ interpret subProgram
  put curState
  return code

execMockedSubShell ::
     (MonadReader PosArgs m, MonadState EnvState m, MonadConsole m, MonadCont m)
  => Program
  -> m String
execMockedSubShell subProgram = execWriterT $ execSubShell subProgram

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

calcDollarExpr ::
     (MonadReader PosArgs m, MonadState EnvState m, MonadConsole m, MonadCont m)
  => DollarExpr
  -> m String
calcDollarExpr (PosArg num)            = asks $ getPosArg num
calcDollarExpr (EnvVar varName)        = gets $ getEnvVar varName
calcDollarExpr (InlineCall subProgram) = trim <$> execMockedSubShell subProgram

calcDoubleQuotes ::
     (MonadReader PosArgs m, MonadState EnvState m, MonadConsole m, MonadCont m)
  => [DQFragment]
  -> m String
calcDoubleQuotes fragments = do
  mappedFragments <- traverse mapper fragments
  return $ concat mappedFragments
  where
    mapper (JustSymbol c)     = return [c]
    mapper (Subst subProgram) = calcDollarExpr subProgram

calcArg ::
     (MonadReader PosArgs m, MonadState EnvState m, MonadConsole m, MonadCont m)
  => Arg
  -> m [String]
calcArg arg = do
  mappedFragments <- traverse mapToMetaChar arg
  let metaString = concat mappedFragments
  let splitedMetaString = wordsBy isMetaSpace metaString
  return $ map (concatMap mapToString) splitedMetaString
  where
    mapToMetaChar (SingleQuotes sq) = return $ pure $ Many sq
    mapToMetaChar (DoubleQuotes dq) = pure . Many <$> calcDoubleQuotes dq
    mapToMetaChar (Symbol c)        = return $ pure $ One c
    mapToMetaChar (Expr e)          = map One <$> calcDollarExpr e
    mapToString (One c)  = [c]
    mapToString (Many s) = s

calcSingleCommand ::
     (MonadReader PosArgs m, MonadState EnvState m, MonadConsole m, MonadCont m)
  => SingleCommand
  -> m [String]
calcSingleCommand command = do
  mappedName <- calcArg $ name command
  mappedArgs <- traverse calcArg $ args command
  return $ mappedName ++ mconcat mappedArgs
