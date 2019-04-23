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
import Control.Monad.Writer (WriterT, execWriterT)
import Data.IORef (IORef, readIORef, writeIORef)
import Enviroment (EnvState (..), MonadConsole (..), PosArgs, getEnvVar, getPosArg)
import Structure (Arg, DollarExpr (..), Program)
import System.Exit (ExitCode)
import System.FilePath (FilePath, isAbsolute, (</>))
import System.IO (hGetContents)
import System.Process (StdStream (CreatePipe), createProcess, cwd, proc, std_out, waitForProcess)

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
  readString = IOConsole $ lift getLine
  writeString s = IOConsole $ lift $ putStrLn s
  callExternal curDir exec args =
    IOConsole $ do
      let processInfo =
            (proc exec args) {cwd = Just curDir, std_out = CreatePipe}
      (_, _, _, procHandle) <- lift $ createProcess processInfo
      lift $ waitForProcess procHandle
  callExternalWithOutput curDir exec args =
    IOConsole $ do
      let processInfo = (proc exec args) {cwd = Just curDir}
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

calcDollarExpr ::
     (MonadReader PosArgs m, MonadState EnvState m, MonadConsole m, MonadCont m)
  => DollarExpr
  -> m String
calcDollarExpr (PosArg num)            = asks $ getPosArg num
calcDollarExpr (EnvVar key)            = gets $ getEnvVar key
calcDollarExpr (InlineCall subProgram) = execMockedSubShell subProgram
