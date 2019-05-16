module Task4
  ( ConcurrentHashTable
  , getCHT
  , newCHT
  , putCHT
  , sizeCHT
  ) where

import           Control.Concurrent.STM.TVar (TVar, newTVar, readTVar,
                                              writeTVar)
import           Control.Monad               (forM_)
import           Control.Monad.STM           (STM, atomically)
import           Data.Hashable               (Hashable, hash)
import           Data.Vector                 ((!))
import qualified Data.Vector                 as DV

type TableElem k v = TVar (Maybe (k, v))

type TableData k v = DV.Vector (TableElem k v)

data ConcurrentHashTable k v =
  ConcurrentHashTable (TVar (TableData k v)) (TVar Int)

defaultSize :: Int
defaultSize = 16

maxLoad :: Double
maxLoad = 0.75

newCHT :: IO (ConcurrentHashTable k v)
newCHT =
  atomically $ do
    vec <- DV.replicateM defaultSize $ newTVar Nothing
    table <- newTVar vec
    sz <- newTVar (0 :: Int)
    return $ ConcurrentHashTable table sz

sizeCHT :: ConcurrentHashTable k v -> IO Int
sizeCHT (ConcurrentHashTable _ c) = atomically $ readTVar c

getElem ::
     (Hashable k, Eq k) => k -> TableData k v -> STM (TableElem k v)
getElem key elems = do
  let cnt = DV.length elems
  let startPos = hash key
  let iter i = do
        let ind = (startPos + i) `mod` cnt
        e <- readTVar (elems ! ind)
        case e of
          Nothing -> return (elems ! ind)
          Just (tk, _) ->
            if tk == key
              then return (elems ! ind)
              else iter (i + 1)
  iter 0

getCHT :: (Hashable k, Eq k) => k -> ConcurrentHashTable k v -> IO (Maybe v)
getCHT key (ConcurrentHashTable t _) =
  atomically $ do
    elems <- readTVar t
    cellTvar <- getElem key elems
    cell <- readTVar cellTvar
    case cell of
      Nothing      -> return Nothing
      Just (_, tv) -> return $ Just tv

getLoad :: Int -> Int -> Double
getLoad cur total = fromIntegral cur / fromIntegral total

putCHT :: (Hashable k, Eq k) => k -> v -> ConcurrentHashTable k v -> IO ()
putCHT key value (ConcurrentHashTable t c) =
  atomically $ do
    elems <- readTVar t
    cnt <- readTVar c
    cellTvar <- getElem key elems
    cell <- readTVar cellTvar
    case cell of
      Just _ -> writeTVar cellTvar $ Just (key, value)
      Nothing ->
        if getLoad (cnt + 1) (DV.length elems) < maxLoad
          then do
            writeTVar cellTvar $ Just (key, value)
            writeTVar c (cnt + 1)
          else do
            newElems <- rehash (key, value) elems
            writeTVar t newElems
            writeTVar c (cnt + 1)

rehash ::
     (Hashable k, Eq k)
  => (k, v)
  -> TableData k v
  -> STM (TableData k v)
rehash extra oldElems = do
  newElems <- DV.replicateM (2 * DV.length oldElems) $ newTVar Nothing
  let putElem (tk, tv) = do
        pos <- getElem tk newElems
        writeTVar pos $ Just (tk, tv)
  DV.forM_ oldElems $ \cellTvar -> do
    cell <- readTVar cellTvar
    forM_ cell putElem
  putElem extra
  return newElems
