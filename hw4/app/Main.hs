module Main where

-- import           Control.Monad (replicateM)
-- import           Data.List     (foldl')
import           System.Random (newStdGen, randomRs)

-- import qualified Task1         as T1
import qualified Task2         as T2

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

main :: IO ()
main = do
  let n = 10000000
  xs <- randomIntList n 1 10
  ys <- randomIntList n 1 10
  let pairs = zip xs ys
  let figure = map (uncurry T2.Point) pairs
  let area = T2.doubleArea figure
  print area
