module Main where

import           System.Environment (getArgs)
import           Task6              (scanDirectory)

main :: IO ()
main = do
  args <- getArgs
  fs <- scanDirectory $ head args
  print fs
