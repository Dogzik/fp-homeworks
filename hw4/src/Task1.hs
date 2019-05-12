{-# LANGUAGE BangPatterns #-}

module Task1
  ( multiply
  , multiplyNaive
  ) where

import           Control.Parallel.Strategies (parListChunk, parMap, rdeepseq,
                                              using)
import           Data.List                   (foldl')
import           Data.Vector.Unboxed         (Vector, fromList, generate,
                                              unsafeIndex, (!))

type Matrix = (Int, Int, Vector Int)

type Multiplier = Matrix -> Matrix -> [[Int]]

listToVectorMatrix :: [[Int]] -> Matrix
listToVectorMatrix mat = do
  let !n = length mat
  let !m = length $ head mat
  (n, m, fromList $ concat mat)

transposeMatrix :: Matrix -> Matrix
transposeMatrix (!ax, !ay, !ma) = (ay, ax, generate (ax * ay) getElem)
  where
    getOrigInd !ind =
      let (!j, !i) = divMod ind ax
       in i * ay + j
    getElem !ind = ma `unsafeIndex` getOrigInd ind

mulArrayMatrixSeq :: Multiplier
mulArrayMatrixSeq (!ax, !ay, ma) (_, !by, !mb) =
  [ [ sum [ma ! (aInd i t) * mb ! (bInd t j) | t <- [0 .. m - 1]]
  | j <- [0 .. k - 1]
  ]
  | i <- [0 .. n - 1]
  ]
  where
    n = ax
    m = ay
    k = by
    aInd x y = x * ay + y
    bInd x y = x * by + y

mulArrayMatrixPar :: Multiplier
mulArrayMatrixPar a b = mulArrayMatrixParInner a (transposeMatrix b)

mulArrayMatrixParInner :: Multiplier
mulArrayMatrixParInner (!ax, !ay, !ma) (!tbx, !tby, !tmb) =
  let !opsPerSpark = 1000000
      !mk = m * k
      idxMatrix = [[(i, j) | j <- [0 .. k - 1]] | i <- [0 .. n - 1]]
   in if n * mk <= opsPerSpark
        then map countRow idxMatrix
        else if mk <= opsPerSpark
               then let !rowsChunk = opsPerSpark `div` mk
                     in map countRow idxMatrix `using`
                        parListChunk rowsChunk rdeepseq
               else let !cellsChunk = max 1 (opsPerSpark `div` m)
                     in parMap
                          (parListChunk cellsChunk rdeepseq)
                          countRow
                          idxMatrix
  where
    n = ax
    m = ay
    k = tbx
    aInd x y = x * ay + y
    bInd x y = x * tby + y
    countProd !i !j !t =
      ma `unsafeIndex` (aInd i t) * tmb `unsafeIndex` (bInd j t)
    countCell (!i, !j) = foldl' (+) 0 $ map (countProd i j) [0 .. m - 1]
    countRow = map countCell

multiplyCore :: Multiplier -> [[Int]] -> [[Int]] -> Maybe [[Int]]
multiplyCore inner ma mb =
  if length (head ma) /= length mb
    then Nothing
    else Just $ inner (listToVectorMatrix ma) (listToVectorMatrix mb)

multiplyNaive :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiplyNaive = multiplyCore mulArrayMatrixSeq

multiply :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiply = multiplyCore mulArrayMatrixPar
