module Main where

import Control.Monad.Cont (callCC, runContT)
import Data.IORef (newIORef)
import Data.Map.Strict (fromList)
import Enviroment (EnvState (..), PosArgs (..))
import Interpreter (interpret)
import IOConsole (MutableEnv (..), runIOConsole)
import Parser (parseProgram)
import Structure (Program)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (..))
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Text.Megaparsec (errorBundlePretty, parse)

main :: IO ()
main = do
  mainArgs <- getArgs
  case mainArgs of
    [] -> print "Path to script and its arguments expected\n"
    path:arguments -> do
      handle <- openFile path ReadMode
      input <- hGetContents handle
      case parse parseProgram "" input of
        Left e -> print $ errorBundlePretty e
        Right script -> do
          _ <- runScript script arguments
          return ()

runScript :: Program -> [String] -> IO ExitCode
runScript script args = do
  let startPosArgs = PosArgs $ fromList $ zip [(1 :: Int) ..] args
  let startEnvVar = fromList []
  startDir <- getCurrentDirectory
  let startEnv = EnvState {curDir = startDir, envVars = startEnvVar}
  envRef <- newIORef startEnv
  let me = MutableEnv {mePosArgs = startPosArgs, meEnvState = envRef}
  runIOConsole (runContT (callCC $ interpret script) return) me

