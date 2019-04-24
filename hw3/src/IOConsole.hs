{-# LANGUAGE MultiParamTypeClasses #-}

module IOConsole
  ( IOConsole(..)
  , MutableEnv(..)
  , runIOConsole
  ) where

import Control.Monad.Catch (SomeException (..), catch)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, local, runReaderT)
import Control.Monad.State (MonadState, get, gets, modify, put)
import Control.Monad.Trans (lift)
import Data.IORef (IORef, readIORef, writeIORef)
import Enviroment (EnvState (..), MonadConsole (..), PosArgs)
import System.Directory (doesDirectoryExist)
import System.Exit (ExitCode (..))
import System.IO (hGetContents)
import System.Process (StdStream (CreatePipe), createProcess, cwd, proc, std_out, waitForProcess)
import Debug.Trace (trace)

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
  readString =
    IOConsole $
    lift (doRead `catch` (\(SomeException _) -> return (ExitFailure 1, "")))
    where
      doRead = do
        str <- getLine
        return (ExitSuccess, str)
  writeString s =
    IOConsole $
    lift (doWrite s `catch` (\(SomeException _) -> return $ ExitFailure 1))
    where
      doWrite s = do
        putStr s
        return ExitSuccess
  directotyExists path = IOConsole $ lift $ doesDirectoryExist path
  callExternal curDir exec args =
    IOConsole $
    lift
      (doCallExternal curDir exec args `catch`
       (\(SomeException _) -> return $ ExitFailure 228))
    where
      doCallExternal curDir exec args = do
        let processInfo = (proc exec args) {cwd = Just curDir}
        (_, _, _, procHandle) <- createProcess processInfo
        waitForProcess procHandle
  callExternalWithOutput curDir exec args =
    IOConsole $
    lift
      (doCallExternalWithOutput curDir exec args `catch`
       (\(SomeException _) -> return (ExitFailure 228, "")))
    where
      doCallExternalWithOutput curDir exec args = do
        let processInfo =
              (proc exec args) {cwd = Just curDir, std_out = CreatePipe}
        (_, Just stdoutHandle, _, procHandle) <- createProcess processInfo
        code <- waitForProcess procHandle
        output <- hGetContents stdoutHandle
        return (code, output)

runIOConsole :: IOConsole a -> MutableEnv -> IO a
runIOConsole (IOConsole inner) = runReaderT inner