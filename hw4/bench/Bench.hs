module Main where

import           Control.DeepSeq (NFData)
import           Control.Monad   (replicateM)
import           Criterion       (bench, nf)
import           Criterion.Main  (Benchmark, bgroup, defaultMain, env)
import           System.Random   (newStdGen, randomRs)
import           Task1           (multiply, multiplyNaive)
import           Task2           (Point (..), doubleArea, doubleAreaNaive,
                                  perimeter, perimeterNaive)

randomListWithBounds :: Int -> Int -> Int -> IO [Int]
randomListWithBounds n from to = take n . randomRs (from, to) <$> newStdGen

randomList :: Int -> IO [Int]
randomList n = randomListWithBounds n 1000 1000

randomMatrix :: Int -> Int -> IO [[Int]]
randomMatrix a b = replicateM a $ randomList b

setupMatrixEnv :: Int -> Int -> Int -> IO ([[Int]], [[Int]])
setupMatrixEnv n m k = do
  m1 <- randomMatrix n m
  m2 <- randomMatrix m k
  return (m1, m2)

setupPointsEnv :: Int -> IO [Point]
setupPointsEnv n = do
  xs <- randomList n
  ys <- randomList n
  return $ zipWith Point xs ys

multiplyBench ::
     Int -> Int -> Int -> ([[Int]] -> [[Int]] -> Maybe [[Int]]) -> Benchmark
multiplyBench n m k op =
  env
    (setupMatrixEnv n m k)
    (\args -> bench (show (n, m, k)) (nf (uncurry op) args))

pointsBench :: (Num a, NFData a) => Int -> ([Point] -> a) -> Benchmark
pointsBench n op = env (setupPointsEnv n) (\args -> bench (show n) (nf op args))

pointsBenches :: (Num a, NFData a) => ([Point] -> a) -> [Benchmark]
pointsBenches op =
  [ pointsBench 1000 op
  , pointsBench 10000 op
  , pointsBench 100000 op
  , pointsBench 1000000 op
  , pointsBench 10000000 op
  ]

matrixBeches :: ([[Int]] -> [[Int]] -> Maybe [[Int]]) -> [Benchmark]
matrixBeches mult =
  [ multiplyBench 10 10 10 mult
  , multiplyBench 50 50 50 mult
  , multiplyBench 100 100 100 mult
  , multiplyBench 2000 300 300 mult
  , multiplyBench 300 2000 300 mult
  , multiplyBench 300 300 2000 mult
  , multiplyBench 1000 1000 1000 mult
  ]

main :: IO ()
main =
  defaultMain
    [ bgroup "multiply" $ matrixBeches multiply
    , bgroup "multiplyNaive" $ matrixBeches multiplyNaive
    , bgroup "perimeter" $ pointsBenches perimeter
    , bgroup "perimeterNaive" $ pointsBenches perimeterNaive
    , bgroup "doubleArea" $ pointsBenches doubleArea
    , bgroup "doubleAreaNaive" $ pointsBenches doubleAreaNaive
    ]
