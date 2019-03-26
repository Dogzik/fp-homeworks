{-# LANGUAGE ScopedTypeVariables #-}

module Block3Spec 
  ( spec
  ) where

import Block3
import Data.List (intercalate)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)

nextChar :: Char -> Char
nextChar c
  | c == maxBound = minBound
  | otherwise     = succ c

spec :: Spec
spec = do
  describe "simple parsers" $ do
    it "ok acceps everything" $ property $
      \(s :: String) -> runParser ok s `shouldBe` Just ((), s)
    it "eof accepts only empty input" $ property $
      \(s :: String, c :: Char) -> let res1 = runParser eof (c:s) 
                                       res2 = runParser eof ""
                                   in (res1, res2) `shouldBe` (Nothing, Just ((), ""))
    it "satisfy checks predicate and returns element for non-empty input" $ property $
      \(s :: String, c :: Char, f :: Bool) -> let res1 = runParser (satisfy (const f)) (c:s)
                                                  res2 = runParser (satisfy (const f)) ""
                                              in if f 
                                                 then (res1, res2) `shouldBe` (Just (c, s), Nothing)
                                                 else (res1, res2) `shouldBe` (Nothing, Nothing)
    it "element picks specific elem from non-empty input" $ property $
      \(s :: String, c :: Char) -> let res1 = runParser (element c) (c:s)
                                       res2 = runParser (element $ nextChar c) (c:s)
                                       res3 = runParser (element c) ""
                                   in (res1, res2, res3) `shouldBe` (Just (c, s), Nothing, Nothing)
    it "stream does same as element, but for list of elems" $ property $ 
      \(s, u :: String, c :: Char) -> let res1 = runParser (stream $ c:u) (c:(u ++ s))
                                          res2 = runParser (stream $ nextChar c:u) (c:(u ++ s))
                                          res3 = runParser (stream $ c:u) ""
                                      in (res1, res2, res3) `shouldBe` (Just (c:u, s), Nothing, Nothing)
  describe "parseCBS accepts string if it is coorect bracket sequence" $ do
    let correct = Just ((), "")
    it "empty is correct" $
      runParser parseCBS "" `shouldBe` correct
    it "simple bracket" $ 
      runParser parseCBS "()" `shouldBe` correct
    it "whitespace suffix make string incorrect" $
      runParser parseCBS "(())() " `shouldBe` Nothing
    it "any whitespace is odd" $
      runParser parseCBS "((()( )))" `shouldBe` Nothing
    it "any other symbol is odd" $
      runParser parseCBS "((()()$))" `shouldBe` Nothing
    it "some big correct sequence" $ 
      runParser parseCBS "((()()))()()()(((())))" `shouldBe` correct
  describe "parseInteger parses integer, with optional `+` or `-` before it, strict version expects eof after" $ do
    it "show X should be parsed to X" $ property $
      \(x :: Integer) -> let x1 = runParser parseInteger $ show x
                             x2 = runParser parseIntegerStrict $ show x
                         in (x1, x2) `shouldBe` (Just (x, ""), Just (x, "")) 
    it "parseIntegerStrict fails without eof" $ property $
      \(x :: Integer, s :: String) -> let x1 = runParser parseInteger $ show x ++ " " ++ s
                                          x2 = runParser parseIntegerStrict $ show x ++ " " ++ s
                                      in (x1, x2) `shouldBe` (Just (x, " " ++ s), Nothing) 
    it "front `+` works too" $ property $
      \(x :: Integer) -> let ax = abs x
                             x1 = runParser parseInteger $ '+' : show ax
                             x2 = runParser parseIntegerStrict $ '+' : show ax
                         in (x1, x2) `shouldBe` (Just (ax, ""), Just (ax, ""))
    it "non-integers fail" $
      runParser parseInteger "I am no an integer" `shouldBe` Nothing
  describe "parseListListInteger parser lists of Integers leaded by their lengths, strict version accepts only whitespace after" $ do
    it "string of whitespaces produces empty list" $
      let s = "  \t \n  "
          res1 = runParser parseListListInteger s
          res2 = runParser parseListListIntegerStrict s
      in (res1, res2) `shouldBe` (Just ([], s), Just ([], ""))
    it "many empty lists works too" $ property $
      \(len :: Int) -> let cnt = len `mod` 100
                           ans = replicate cnt []
                           s = intercalate ", " (map (show . length) ans)
                       in runParser parseListListInteger s `shouldBe` Just (ans, "")
    it "random list of integers" $ property $
      \(listList :: [[Integer]]) -> let listToString l = intercalate ", " (map show $ toInteger (length l):l)
                                        s = intercalate ", " (map listToString listList)
                                    in runParser parseListListIntegerStrict s `shouldBe` Just (listList, "")
    it "strict fails on smaller length" $ property $
      \(list :: [Integer]) -> let l = (5:take 100 list) ++ [1000]
                                  len = length l - 1
                                  s = intercalate ", " (map show $ toInteger len:l)
                                  res1 = runParser parseListListIntegerStrict s
                                  res2 = runParser parseListListInteger s 
                                  ans = take len l
                                  rest = ", " ++ show (last l)
                              in (res1, res2) `shouldBe` (Nothing, Just ([ans], rest))
    it "too big list size" $ property $
      \(list :: [Integer]) -> let l = take 100 list
                                  s = intercalate ", " (map show $ toInteger (length l + 1):l)
                              in runParser parseListListIntegerStrict s `shouldBe` Nothing
                            