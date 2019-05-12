{-# LANGUAGE BangPatterns #-}

module Task2
  ( Point(..)
  , crossProduct
  , doubleArea
  , doubleAreaNaive
  , minus
  , perimeter
  , perimeterNaive
  , plus
  , scalarProduct
  ) where

import           Control.DeepSeq (NFData, rnf)

data Point =
  Point
    { x :: !Int
    , y :: !Int
    }
  deriving (Show)

instance NFData Point where
  rnf (Point !ax !ay) = ax `seq` (ay `seq` ())

coordFunc :: (Int -> Int -> Int) -> Point -> Point -> Point
coordFunc !f (Point ax ay) (Point bx by) =
  let cx = f ax bx
      cy = f ay by
   in Point cx cy

plus :: Point -> Point -> Point
plus = coordFunc (+)

minus :: Point -> Point -> Point
minus = coordFunc (-)

scalarProduct :: Point -> Point -> Int
scalarProduct (Point ax ay) (Point bx by) = ax * bx + ay * by

crossProduct :: Point -> Point -> Int
crossProduct (Point ax ay) (Point bx by) = ax * by - ay * bx

vecLength :: Point -> Point -> Double
vecLength a b =
  let diff = a `minus` b
   in sqrt $ fromIntegral $ scalarProduct diff diff

sumMap :: Num a => (Point -> Point -> a) -> [Point] -> a
sumMap _ [] = 0
sumMap !func list@(a:_) = inner func a list 0
  where
    inner _ _ [] !acc             = acc
    inner f begin [end] !acc      = inner f begin [] (acc + f end begin)
    inner f begin (x1:x2:xs) !acc = inner f begin (x2 : xs) (acc + f x1 x2)

sumMapNaive :: Num a => (Point -> Point -> a) -> [Point] -> a
sumMapNaive _ []   = 0
sumMapNaive f list = sum $ zipWith f list (tail $ cycle list)

perimeter :: [Point] -> Double
perimeter = sumMap vecLength

perimeterNaive :: [Point] -> Double
perimeterNaive = sumMapNaive vecLength

doubleArea :: [Point] -> Int
doubleArea figure = abs $ sumMap crossProduct figure

doubleAreaNaive :: [Point] -> Int
doubleAreaNaive figure = abs $ sumMapNaive crossProduct figure
