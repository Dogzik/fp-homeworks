{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

module Enviroment
  ( MonadConsole(..)
  , PosArgs
  , EnvState(..)
  , getPosArg
  , getEnvVar
  ) where

import Control.Monad.Cont (ContT)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Writer (WriterT, tell)
import Data.Map.Strict (Map, findWithDefault, insert)
import System.Exit (ExitCode)

newtype PosArgs =
  PosArgs (Map Int String)

data EnvState = EnvState
  { curDir  :: FilePath
  , envVars :: Map String String
  } deriving (Show)

getPosArg :: Int -> PosArgs -> String
getPosArg num (PosArgs args) = findWithDefault "" num args

getEnvVar :: String -> EnvState -> String
getEnvVar key env = findWithDefault "" key $ envVars env

class Monad m =>
      MonadConsole m
  where
  readString :: m String
  writeString :: String -> m ()
  callExternal :: FilePath -> FilePath -> [String] -> m ExitCode
  callExternalWithOutput ::
       FilePath -> FilePath -> [String] -> m (ExitCode, String)

instance MonadConsole m => MonadConsole (WriterT String m) where
  readString = lift readString
  writeString s = tell $ s ++ " "
  callExternal cwd exec args = do
    (code, output) <- lift $ callExternalWithOutput cwd exec args
    tell output
    return code
  callExternalWithOutput cwd exec args =
    lift $ callExternalWithOutput cwd exec args

instance MonadConsole m => MonadConsole (ContT r m) where
  readString = lift readString
  writeString = lift . writeString
  callExternal cwd exec args = lift $ callExternal cwd exec args
  callExternalWithOutput cwd exec args =
    lift $ callExternalWithOutput cwd exec args
-- class Monad m =>
--       MonadMockedOutput mm m
--   | m -> mm
--   where
--   getOutput :: m a -> mm String
--   sameReadString :: m String
--   mockedWriteString :: String -> m ()
--   mockedCallExternal :: FilePath -> FilePath -> [String] -> m ExitCode
--   sameCallExternalWithOutput ::
--        FilePath -> FilePath -> [String] -> m (ExitCode, String)
-- instance forall m. MonadConsole m =>
--          MonadMockedOutput m (WriterT String m) where
--   sameReadString = lift readString
--   mockedWriteString s = tell $ s ++ " "
--   getOutput = execWriterT
--   mockedCallExternal cwd exec args = do
--     (code, output) <- lift $ callExternalWithOutput cwd exec args
--     tell output
--     return code
--   sameCallExternalWithOutput cwd exec args =
--     lift $ callExternalWithOutput cwd exec args
-- instance (Monad m, MonadMockedOutput mm m) => MonadConsole m where
--   readString = sameReadString
--   writeString = mockedWriteString
--   callExternal = mockedCallExternal
--   callExternalWithOutput = sameCallExternalWithOutput
