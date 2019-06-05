{-# LANGUAGE ScopedTypeVariables #-}

module Block1Spec
  ( spec
  ) where

import           Task1           (newSTRef, readSTRef, runST, writeSTRef)
import           Test.Hspec      (Spec, describe, it, shouldBe)
import           Test.QuickCheck (property)

whileM :: Monad m => m Bool -> m () -> m ()
whileM c act =
  c >>= \b ->
    if b
      then act >> whileM c act
      else pure ()

fib :: Int -> Int
fib x
  | x <= 2 = 1
  | otherwise = fib (x - 1) + fib (x - 2)

fib' :: Int -> Int
fib' x =
  runST $ do
    a <- newSTRef 1
    b <- newSTRef 1
    n <- newSTRef 2
    let cond = do
          cur <- readSTRef n
          return (cur < x)
    whileM cond $ do
      a' <- readSTRef a
      b' <- readSTRef b
      n' <- readSTRef n
      writeSTRef a b'
      writeSTRef b (a' + b')
      writeSTRef n (n' + 1)
    readSTRef b

eps :: Double
eps = 1.0e-8

sqrt' :: Double -> Double
sqrt' x
  | x < 1 = error "x < 1 not supported"
  | x == 0 = 0
  | otherwise =
    runST $ do
      l <- newSTRef 0
      r <- newSTRef x
      let checkCond = do
            l_ <- readSTRef l
            r_ <- readSTRef r
            pure (r_ - l_ > eps)
      whileM checkCond $ do
        l_ <- readSTRef l -- l^2 < x
        r_ <- readSTRef r -- r^2 >= x
        let m = (l_ + r_) / 2
        if (m * m >= x)
          then writeSTRef r m
          else writeSTRef l m
      readSTRef r

spec :: Spec
spec =
  describe "testing my \"ST\" monad" $ do
    it "testing fib" $
      property $ \(x :: Int) ->
        let xx = x `mod` 20
         in fib' xx `shouldBe` fib xx
    it "testing sqrt" $
      property $ \(x :: Int) ->
        let xx = fromIntegral $ abs (x `mod` 500) + 1
         in abs (sqrt xx - sqrt' xx) <= eps `shouldBe` True
