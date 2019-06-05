{-# LANGUAGE ScopedTypeVariables #-}

module Block4Spec
  ( spec
  ) where

import           Task2           (Number (..), Val (..), runSTJS1)
import           Task4           (factJS, fibJS, sqrtIntJS)
import           Test.Hspec      (Spec, describe, it, shouldBe)
import           Test.QuickCheck (property)

fib :: Int -> Int
fib x
  | x <= 2 = 1
  | otherwise = fib (x - 1) + fib (x - 2)

fact :: Int -> Int
fact x
  | x <= 1 = 1
  | otherwise = x * fact (x - 1)

sqrtInt :: Int -> Int
sqrtInt x =
  if x < 0
    then 0
    else inner 0 (x + 1)
  where
    inner l r
      | r - l <= 1 = l
      | otherwise =
        let m = (l + r) `div` 2
         in if m * m > x
              then inner l m
              else inner m r

spec :: Spec
spec =
  describe "testing my \"JS\" functions" $ do
    it "testing fib" $
      property $ \(x :: Int) ->
        let xx = x `mod` 20
         in runSTJS1 xx fibJS `shouldBe` (Numb $ Inter $ fib xx)
    it "testing fact" $
      property $ \(x :: Int) ->
        let xx = x `mod` 200
         in runSTJS1 xx factJS `shouldBe` (Numb $ Inter $ fact xx)
    it "testing sqrt" $
      property $ \(x :: Int) ->
        runSTJS1 x sqrtIntJS `shouldBe` (Numb $ Inter $ sqrtInt x)
