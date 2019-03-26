{-# LANGUAGE ScopedTypeVariables #-}

module Block1Spec 
  ( spec
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)
import Block1 (stringSum)
import Data.List (intercalate)

spec :: Spec
spec = do
  describe "stringSum returns sum of ints in string, if it consists of valid ints" $ do
    it "one int equals intself" $  property $ 
      \(x :: Int) -> stringSum (show x) `shouldBe` Just x
    it "empty string equals zero" $
      stringSum "" `shouldBe` Just 0
    it "random list of ints works too" $ property $
      \(xs :: [Int]) -> let expectedSum = Just (sum xs)
                            str = intercalate " " (map show xs)
                        in stringSum str `shouldBe` expectedSum
    it "odd chars should produce Nothing" $
      stringSum "1 3 4 ##@ 3" `shouldBe` Nothing
        
