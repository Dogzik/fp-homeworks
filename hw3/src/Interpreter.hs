{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Interpreter
  ( interpret
  ) where

import Control.Monad (mapM_)
import Control.Monad.Catch (SomeException (..), catch)
import Control.Monad.Cont (MonadCont, callCC)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, local)
import Control.Monad.State (MonadState, get, gets, modify, put)
import Control.Monad.Writer (WriterT, execWriterT)
import Data.Char (isSpace)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.Split (wordsBy)
import Data.Map.Strict (insert)
import Enviroment (EnvState (..), MonadConsole (..), PosArgs, getEnvVar, getPosArg)
import Parser (isIdentifier)
import Structure (Arg, ArgFragment (..), Assignment (..), Command (..), DQFragment (..),
                  DollarExpr (..), Program, SingleCommand (..))
import System.Exit (ExitCode (..))
import System.FilePath (FilePath, isAbsolute, (</>))
import Text.Read (readMaybe)

import Debug.Trace (trace)

data MetaChar
  = One Char
  | Many String
  deriving (Show)

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
  execCommand command hook
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
    name:args ->
      case name of
        "echo" -> execEcho args
        "pwd" -> execPwd args
        "cd" -> execCd args
        "exit" -> execExit args hook
        "read" -> execRead args
        _ -> do
          dir <- gets curDir
          callExternal dir name args
execCommand _ _ = error "Kok"

execEcho ::
     (MonadReader PosArgs m, MonadState EnvState m, MonadConsole m, MonadCont m)
  => [String]
  -> m ExitCode
execEcho [] = writeString "\n"
execEcho args@(x:xs) =
  if x == "-n"
    then writeString $ unwords xs
    else writeString $ unwords args ++ "\n"

execPwd ::
     (MonadReader PosArgs m, MonadState EnvState m, MonadConsole m, MonadCont m)
  => [String]
  -> m ExitCode
execPwd args =
  case args of
    [] -> do
      dir <- gets curDir
      writeString $ dir ++ "\n"
    _ -> do
      writeString "Too many arguments for pwd\n"
      return $ ExitFailure 2

execCd ::
     (MonadReader PosArgs m, MonadState EnvState m, MonadConsole m, MonadCont m)
  => [String]
  -> m ExitCode
execCd args =
  case args of
    [] -> do
      writeString "Too few arguments for cd\n"
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
          writeString ("No such directory: " ++ newDir ++ "\n")
          return $ ExitFailure 2
    _ -> do
      writeString "Too many arguments for cd\n"
      return $ ExitFailure 2

execExit ::
     (MonadReader PosArgs m, MonadState EnvState m, MonadConsole m, MonadCont m)
  => [String]
  -> (ExitCode -> m ExitCode)
  -> m ExitCode
execExit args hook =
  case args of
    [] -> do
      writeString "Too few arguments for exit\n"
      return $ ExitFailure 3
    [arg] ->
      case readMaybe arg of
        Nothing -> do
          writeString ("Wrong argument for exit: " ++ arg ++ "\n")
          return $ ExitFailure 1
        Just x ->
          if x == 0
            then hook ExitSuccess
            else hook $ ExitFailure x
    _ -> do
      writeString "Too many arguments for exit\n"
      return $ ExitFailure 2

setEnvVar ::
     (MonadReader PosArgs m, MonadState EnvState m, MonadConsole m, MonadCont m)
  => String
  -> String
  -> m ExitCode
setEnvVar key value =
  if isIdentifier key
    then do
      curEnvVars <- gets envVars
      let newEnvVars = insert key value curEnvVars
      modify (\s -> s {envVars = newEnvVars})
      return ExitSuccess
    else do
      writeString (key ++ " is not a valid identifier\n")
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
      writeString "Too many argumens for assignment\n"
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
doReadAssign (var:vars) (value:values) = do
  code <- setEnvVar var value
  case code of
    ExitSuccess   -> doReadAssign vars values
    ExitFailure x -> return $ ExitFailure x

execRead ::
     (MonadReader PosArgs m, MonadState EnvState m, MonadConsole m, MonadCont m)
  => [String]
  -> m ExitCode
execRead args = do
  (code, input) <- readString
  case code of
    ExitFailure x -> return $ ExitFailure x
    ExitSuccess -> do
      let strings = words input
      doReadAssign args strings

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

trimLeft :: String -> String
trimLeft = dropWhile isSpace

trimRight :: String -> String
trimRight s = reverse $ trimLeft $ reverse s

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

calcDollarExpr ::
     (MonadReader PosArgs m, MonadState EnvState m, MonadConsole m, MonadCont m)
  => DollarExpr
  -> m String
calcDollarExpr (PosArg num)            = asks $ getPosArg num
calcDollarExpr (EnvVar key)            = gets $ getEnvVar key
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
