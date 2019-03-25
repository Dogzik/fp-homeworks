{-# LANGUAGE ScopedTypeVariables #-}

module Block2Spec 
  ( spec
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)
import Block2 (ArithmeticError (..), Expr (..), eval, moving)

spec :: Spec
spec = do
  describe "eval evaluates expresseion represented by Expr class" $ do
    it "Const X is always X" $
      property $
        \(x :: Int) -> eval (Const x) `shouldBe` Right x
    it "Add, Sub and Mul work as +, - and *" $
      property $
        \(a, b, c, d, e, f :: Int) -> let x = Mul (Const a) (Const b)
                                          y = Add (Const c) (Const d)
                                          z = Mul (Const e) (Const f)
                                      in eval (Add x (Sub y z)) `shouldBe` Right ((a * b) + ((c + d) - (e * f)))
    it "Div with second zero always fails" $
      property $
        \(a, b, c :: Int) -> eval (Div (Mul (Const a) (Const b)) (Sub (Const c) (Const c))) `shouldBe` Left DivisionByZero
    it "but Div works with non-zero second similar to `div`" $
      property $
        \(a, b :: Int) -> let res = eval $ Div (Const a) (Const b)
                          in if b == 0
                             then res `shouldBe` Left DivisionByZero
                             else res `shouldBe` Right (a `div` b)
    it "Pow is afraid of negtive exponent, but works as ^ otherwise" $
      property $
        \(a, b :: Int) -> let res = eval $ Pow (Const a) (Const b)
                          in if b < 0
                             then res `shouldBe` Left NegativeExponent
                             else res `shouldBe` Right (a ^ b)
  describe "moving calculates Simple Moving Average if given window size" $ do
    it "moving 4 [1, 5, 3, 8, 7, 9, 6] == [1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5]" $
      moving 4 [1, 5, 3, 8, 7, 9, 6] `shouldBe` ([1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5] :: [Double])
    it "moving 2 [1, 5, 3, 8, 7, 9, 6] == [1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]" $
      moving 2 [1, 5, 3, 8, 7, 9, 6] `shouldBe` ([1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5] :: [Double])

