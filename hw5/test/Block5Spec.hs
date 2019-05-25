{-# LANGUAGE ScopedTypeVariables #-}

module Block5Spec
  ( spec
  ) where

import           Task5           (over, set, view, (%~), _1, _2)
import           Test.Hspec      (Spec, describe, it, shouldBe)
import           Test.QuickCheck (property)

spec :: Spec
spec =
  describe "testing some primitive lens" $ do
    it "geting first element from pair" $
      property $ \(pair@(a, _) :: (Int, String)) -> view _1 pair `shouldBe` a
    it "setting last element in pair" $
      property $ \(pair@(a, _) :: (Char, Double)) (nb :: Double) ->
        set _2 nb pair `shouldBe` (a, nb)
    it "updating both elements" $
      property $ \(pair@(a, b) :: (Int, Bool)) ->
        over _1 (+ 1) ((_2 %~ not) pair) `shouldBe` (a + 1, not b)
