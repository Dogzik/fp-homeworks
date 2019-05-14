{-# LANGUAGE BangPatterns #-}

module Task3
  ( gauss
  , verifySolution
  ) where

import           Control.DeepSeq             (($!!))
import           Control.Monad               (forM_, when)
import           Control.Monad.ST            (ST, runST)
import           Control.Parallel.Strategies (parListChunk, rdeepseq, using)
import           Data.Bits                   (xor)
import           Data.List                   (foldl')
import qualified Data.Vector                 as DV
import qualified Data.Vector.Mutable         as DVM
import qualified Data.Vector.Unboxed         as DVU
import qualified Data.Vector.Unboxed.Mutable as DVUM

verifySolution :: [[Bool]] -> [Bool] -> [Bool] -> Bool
verifySolution a b x =
  let opsPerSpark = 1000000
      !n = length a
      zipped = zip a b
      strategy =
        if n * n <= opsPerSpark
          then rdeepseq
          else let !chunkSize = max 1 (opsPerSpark `div` n)
                in parListChunk chunkSize rdeepseq
   in let checks = map mapLine zipped `using` strategy
       in foldl' (&&) True checks
  where
    mapLine (line, end) = end == foldl' xor False (zipWith (&&) line x)

gauss :: [[Bool]] -> [Bool] -> Maybe [Bool]
gauss a b =
  runST $ do
    system <- listsToVectors a b
    ans <- gaussInner system
    if verifySolution a b ans
      then return $!! Just ans
      else return Nothing

type MVector2D s = DVM.STVector s (DVUM.STVector s Bool)

type MMatrix s = (Int, Int, MVector2D s)

listsToVectors :: [[Bool]] -> [Bool] -> ST s (MMatrix s)
listsToVectors a b = do
  let !n = length a
  let !m = length $ head a
  let extentedA = zipWith (++) a (map pure b)
  listOfVectors <- mapM listToVec extentedA
  vectorOfVectors <- DV.unsafeThaw $!! DV.fromList listOfVectors
  return (n, m, vectorOfVectors)
  where
    listToVec list = DVU.unsafeThaw $!! DVU.fromList list

getElem :: Int -> Int -> MVector2D s -> ST s Bool
getElem !i !j !mat = do
  row <- DVM.read mat i
  DVUM.read row j

subRows :: DVUM.STVector s Bool -> DVUM.STVector s Bool -> ST s ()
subRows !a !b = do
  let m = DVUM.length a
  forM_ [0 .. m - 1] $ \j -> do
    x <- DVUM.read b j
    DVUM.modify a (`xor` x) j

columnIter :: Int -> Int -> MMatrix s -> ST s [Maybe Int]
columnIter !curColumn !curRow system@(!n, !m, !mat)
  | curColumn == m = return []
  | curRow == n = do
    xs <- columnIter (curColumn + 1) curRow system
    return $!! Nothing : xs
  | otherwise = do
    forM_ [curRow .. n - 1] $ \i -> do
      tmp <- getElem i curColumn mat
      when tmp $! DVM.swap mat curRow i
    e <- getElem curRow curColumn mat
    if not e
      then do
        xs <- columnIter (curColumn + 1) curRow system
        return $!! Nothing : xs
      else do
        rb <- DVM.read mat curRow
        forM_ [0 .. n - 1] $ \i ->
          when (i /= curRow) $! do
            ra <- DVM.read mat i
            marker <- getElem i curColumn mat
            when marker $! subRows ra rb
        xs <- columnIter (curColumn + 1) (curRow + 1) system
        return $!! Just curRow : xs

gaussInner :: MMatrix s -> ST s [Bool]
gaussInner system@(_, !m, !mat) = do
  ids <- columnIter 0 0 system
  ans <- mapM getAnsElem ids
  return $!! ans
  where
    getAnsElem Nothing  = return False
    getAnsElem (Just i) = getElem i m mat
