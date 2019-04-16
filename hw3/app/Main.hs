module Main where

import Parser
import System.IO
import Text.Megaparsec

main :: IO ()
main = do
  handle <- openFile "/home/dogzik/script.sh" ReadMode
  input <- hGetContents handle
  let res = runParser parseSingleQuote "" input
  --parseTest parseSingleQuote input
  case res of
    Left a  -> print a
    Right b -> putStr b
