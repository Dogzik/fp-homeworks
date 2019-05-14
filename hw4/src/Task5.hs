{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Task5
  ( AllocateT(..)
  , allocate
  , release
  , resourceFork
  , runAllocateT
  ) where

import           Control.Applicative         (liftA2)
import           Control.Concurrent.STM.TVar (TVar, newTVar, stateTVar)
import           Control.Monad               (forM_)
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow,
                                              SomeException, bracket, catch,
                                              catchAll, generalBracket, mask,
                                              mask_, throwM,
                                              uninterruptibleMask)
import           Control.Monad.Reader        (MonadReader, ReaderT (ReaderT),
                                              ask, asks, local, runReaderT)
import           Control.Monad.STM           (atomically)
import           Control.Monad.Trans         (MonadIO, MonadTrans, lift, liftIO)
import           Data.Either                 (partitionEithers)
import           Focus                       (adjust, lookupAndDelete)
import           ListT                       (toList, traverse_)
import           StmContainers.Map           (Map)
import qualified StmContainers.Map           as StmMap

type ResourceKey = Integer

data Env =
  Env
    { counter   :: TVar ResourceKey
    , resources :: Map ResourceKey (Int, IO ())
    }

newtype AllocateT m a =
  AllocateT (ReaderT Env m a)

unpack :: AllocateT m a -> ReaderT Env m a
unpack (AllocateT x) = x

instance (Monad m) => Functor (AllocateT m) where
  fmap f funX = do
    x <- funX
    return $ f x

instance (Monad m) => Applicative (AllocateT m) where
  pure = return
  apF <*> apX = do
    f <- apF
    x <- apX
    return $ f x

instance (Monad m) => Monad (AllocateT m) where
  return = AllocateT . return
  (AllocateT monad) >>= f = AllocateT $ monad >>= (unpack . f)

instance MonadTrans AllocateT where
  lift act = AllocateT $ lift act

instance (Monad m) => MonadReader Env (AllocateT m) where
  ask = AllocateT ask
  local f (AllocateT monad) = AllocateT $ local f monad

instance (MonadThrow m) => MonadThrow (AllocateT m) where
  throwM e = lift $ throwM e

instance (MonadCatch m) => MonadCatch (AllocateT m) where
  catch (AllocateT monad) f = AllocateT $ monad `catch` (unpack . f)

instance (MonadMask m) => MonadMask (AllocateT m) where
  mask a = AllocateT $ mask $ \u -> unpack (a $ q u)
    where
      q u = AllocateT . u . unpack
  uninterruptibleMask a =
    AllocateT $ uninterruptibleMask $ \u -> unpack (a $ q u)
    where
      q u = AllocateT . u . unpack
  generalBracket a r u =
    AllocateT $
    generalBracket
      (unpack a)
      (\resource exitCase -> unpack (r resource exitCase))
      (unpack . u)

release :: (MonadIO m, MonadMask m) => ResourceKey -> AllocateT m ()
release key =
  mask_ $ do
    resourse <- getResourse
    case resourse of
      Nothing           -> return ()
      Just (_, deleter) -> lift $ liftIO deleter
  where
    getResourse = do
      envMap <- asks resources
      lift $ liftIO $ atomically $ StmMap.focus lookupAndDelete key envMap

allocate ::
     (MonadIO m, MonadMask m)
  => IO a
  -> (a -> IO ())
  -> AllocateT m (a, ResourceKey)
allocate alloc delete =
  mask_ $ do
    resource <- lift $ liftIO alloc
    addResource resource `catchAll` handleError resource
  where
    nextIndState x =
      let next = succ x
       in (next, next)
    addResource r = do
      ind <- asks counter
      envMap <- asks resources
      lift $
        liftIO $
        atomically $ do
          newInd <- stateTVar ind nextIndState
          StmMap.insert (1, delete r) newInd envMap
          return (r, newInd)
    handleError r e = do
      lift $ liftIO $ delete r
      throwM e

tryAll :: (MonadCatch m) => m a -> m (Either SomeException a)
tryAll act = (Right <$> act) `catchAll` (return . Left)

clearResources :: (MonadIO m, MonadCatch m) => Env -> m ()
clearResources Env {resources = mapData} =
  liftIO $ do
    deleters <- getDeleters
    results <- mapM tryAll deleters
    case partitionEithers results of
      ([], _)  -> return ()
      (e:_, _) -> throwM e
  where
    getDeleters =
      atomically $ do
        elems <- toList $ StmMap.listT mapData
        let isUnique (_, (cnt, _)) = cnt == 1
        let unique = map (snd . snd) (filter isUnique elems)
        let getMeta (key, (cnt, _)) = (key, cnt)
        let meta = map getMeta elems
        let act (key, cnt) =
              if cnt == 1
                then StmMap.delete key mapData
                else StmMap.focus (adjust (\(c, d) -> (c - 1, d))) key mapData
        forM_ meta act
        return unique

runAllocateT :: (MonadIO m, MonadMask m) => AllocateT m a -> m a
runAllocateT (AllocateT action) =
  bracket createMap clearResources (runReaderT action)
  where
    createMap = liftIO $ atomically $ liftA2 Env (newTVar 0) StmMap.new

resourceFork ::
     (MonadIO m, MonadMask m)
  => (m () -> m ())
  -> AllocateT m ()
  -> AllocateT m ()
resourceFork fork (AllocateT act) =
  AllocateT $ bracket updMap clearResources doFork
  where
    updMap = do
      mapData <- asks resources
      lift $
        liftIO $
        atomically $
        let listT = StmMap.listT mapData
            actM (key, (_, _)) =
              StmMap.focus (adjust (\(c, d) -> (c + 1, d))) key mapData
         in traverse_ actM listT
      ask
    doFork env =
      ReaderT $ \_ ->
        let newAct = runReaderT act env
         in fork newAct
