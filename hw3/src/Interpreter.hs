{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- {-# LANGUAGE UndecidableInstances  #-}
module Interpreter
  ( interpret
  ) where

import Control.Monad.Cont (MonadCont, callCC)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, local)
import Control.Monad.State (MonadState, get, gets, put)
import Control.Monad.Trans (lift)
import Control.Monad.Catch (catch, SomeException (..))
import Control.Monad.Writer (WriterT, execWriterT)
import Data.Char (isSpace)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.Split (wordsBy)
import Enviroment (EnvState (..), MonadConsole (..), PosArgs, getEnvVar, getPosArg)
import Structure (Arg, ArgFragment (..), DQFragment (..), DollarExpr (..), Program,
                  SingleCommand (..))
import System.Exit (ExitCode (..))
import System.FilePath (FilePath, isAbsolute, (</>))
import System.IO (hGetContents)
import System.Process (StdStream (CreatePipe), createProcess, cwd, proc, std_out, waitForProcess)

data MetaChar
  = One Char
  | Many String
  deriving (Show)

isMetaSpace (One c) = isSpace c
isMetaSpace _       = False

data MutableEnv = MutableEnv
  { mePosArgs  :: PosArgs
  , meEnvState :: IORef EnvState
  }

newtype IOConsole a =
  IOConsole (ReaderT MutableEnv IO a)

unpack :: IOConsole a -> ReaderT MutableEnv IO a
unpack (IOConsole x) = x

instance Functor IOConsole where
  fmap f funX = do
    x <- funX
    return $ f x

instance Applicative IOConsole where
  pure = return
  apF <*> apX = do
    f <- apF
    x <- apX
    return $ f x

instance Monad IOConsole where
  return = IOConsole . return
  (IOConsole m) >>= f = IOConsole $ m >>= (unpack . f)

instance MonadReader PosArgs IOConsole where
  ask = IOConsole $ asks mePosArgs
  local f (IOConsole m) = IOConsole $ local (updPosArgs f) m
    where
      updPosArgs f mEnv =
        let newPosArgs = f $ mePosArgs mEnv
         in mEnv {mePosArgs = newPosArgs}

instance MonadState EnvState IOConsole where
  get =
    IOConsole $ do
      stateRef <- asks meEnvState
      lift $ readIORef stateRef
  put newState =
    IOConsole $ do
      stateRef <- asks meEnvState
      lift $ writeIORef stateRef newState

instance MonadConsole IOConsole where
  readString = IOConsole $ doRead `catch` (\(SomeException _) -> return (ExitFailure 1, ""))
    where
      doRead = do
        str <- lift getLine
        return (ExitSuccess, str)
  writeString s = IOConsole $ doWrite s `catch` (\(SomeException _) -> return $ ExitFailure 1) 
    where
      doWrite s = do
        lift $ putStrLn s
        return ExitSuccess 
  callExternal curDir exec args =
    IOConsole $ do
      let processInfo = (proc exec args) {cwd = Just curDir}
      (_, _, _, procHandle) <- lift $ createProcess processInfo
      lift $ waitForProcess procHandle
  callExternalWithOutput curDir exec args =
    IOConsole $ do
      let processInfo =
            (proc exec args) {cwd = Just curDir, std_out = CreatePipe}
      (_, Just stdoutHandle, _, procHandle) <- lift $ createProcess processInfo
      code <- lift $ waitForProcess procHandle
      output <- lift $ hGetContents stdoutHandle
      return (code, output)

interpret ::
     (MonadReader PosArgs m, MonadState EnvState m, MonadConsole m, MonadCont m)
  => Program
  -> (ExitCode -> m b)
  -> m ExitCode
interpret = undefined

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
