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
import           Control.Concurrent.MVar     (MVar, newEmptyMVar, putMVar,
                                              takeMVar)
import           Control.Concurrent.STM.TVar (TVar, newTVar, stateTVar)
import           Control.Monad               (forM_)
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow,
                                              SomeException, catch, catchAll,
                                              finally, generalBracket, mask,
                                              mask_, throwM,
                                              uninterruptibleMask)
import           Control.Monad.Reader        (MonadReader, ReaderT, ask, asks,
                                              local, runReaderT)
import           Control.Monad.STM           (atomically)
import           Control.Monad.Trans         (MonadIO, MonadTrans, lift, liftIO)
import           Data.Either                 (partitionEithers)
import           Data.Hashable               (Hashable, hashWithSalt)
import           Data.IORef                  (IORef, modifyIORef, newIORef,
                                              readIORef)
import           Data.Maybe                  (catMaybes)
import qualified Focus
import           StmContainers.Map           (Map)
import qualified StmContainers.Map           as StmMap

newtype ResourceKey =
  ResourceKey Integer

instance Eq ResourceKey where
  ResourceKey a == ResourceKey b = a == b

instance Hashable ResourceKey where
  hashWithSalt salt (ResourceKey a) = hashWithSalt salt a

instance Enum ResourceKey where
  toEnum x = ResourceKey $ toEnum x
  fromEnum (ResourceKey x) = fromEnum x

data CommonEnv =
  CommonEnv
    { counter   :: TVar ResourceKey
    , resources :: Map ResourceKey (Int, IO ())
    }

data ThreadEnv =
  ThreadEnv
    { common          :: CommonEnv
    , threadResources :: IORef [ResourceKey]
    , threadJoints    :: IORef [MVar ()]
    }

newtype AllocateT m a =
  AllocateT (ReaderT ThreadEnv m a)

unpack :: AllocateT m a -> ReaderT ThreadEnv m a
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

instance (MonadIO m) => MonadIO (AllocateT m) where
  liftIO act = AllocateT $ liftIO act

instance (Monad m) => MonadReader ThreadEnv (AllocateT m) where
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
      envMap <- asks (resources . common)
      lift $ liftIO $ atomically $ StmMap.focus Focus.lookupAndDelete key envMap

addElem :: IORef [a] -> a -> IO ()
addElem list e = modifyIORef list (e :)

allocate ::
     (MonadIO m, MonadMask m)
  => IO a
  -> (a -> IO ())
  -> AllocateT m (a, ResourceKey)
allocate alloc delete = do
  resource <- liftIO alloc
  ind <- asks (counter . common)
  envMap <- asks (resources . common)
  mask_ $ action ind envMap resource `catchAll` handleError resource
  where
    nextIndState x =
      let next = succ x
       in (next, next)
    handleError r e = do
      liftIO $ delete r
      throwM e
    innerHandle key envMap e = do
      liftIO $ atomically $ StmMap.delete key envMap
      throwM e
    innerAction resource key = do
      myResourcesRef <- asks threadResources
      lift $ liftIO $ addElem myResourcesRef key
      return (resource, key)
    action ind envMap r = do
      (resource, key) <-
        liftIO $
        atomically $ do
          newInd <- stateTVar ind nextIndState
          StmMap.insert (1, delete r) newInd envMap
          return (r, newInd)
      innerAction resource key `catchAll` innerHandle key envMap

tryAll :: (MonadCatch m) => m a -> m (Either SomeException a)
tryAll act = (Right <$> act) `catchAll` (return . Left)

clearResources :: (MonadIO m, MonadCatch m) => ThreadEnv -> m ()
clearResources ThreadEnv { common = CommonEnv {resources = mapData}
                         , threadResources = keysRef
                         } =
  liftIO $ do
    deleters <- getDeleters
    results <- mapM tryAll deleters
    case partitionEithers results of
      ([], _)  -> return ()
      (e:_, _) -> throwM e
  where
    getDeleters = do
      keys <- readIORef keysRef
      atomically $ do
        let uniqueFocus = do
              t <- Focus.lookup
              case t of
                Just (1, d) -> return $ Just d
                _           -> return Nothing
        uniqueJusts <- mapM (\key -> StmMap.focus uniqueFocus key mapData) keys
        let unique = catMaybes uniqueJusts
        let updateFocus = do
              t <- Focus.lookup
              case t of
                Nothing -> return ()
                Just (c, d) ->
                  if c == 1
                    then Focus.delete
                    else Focus.adjust $ const (c - 1, d)
        mapM_ (\key -> StmMap.focus updateFocus key mapData) keys
        return unique

runAllocateT :: (MonadIO m, MonadMask m) => AllocateT m a -> m a
runAllocateT (AllocateT action) = do
  newCommon <-
    liftIO $ atomically $ liftA2 CommonEnv (newTVar $ ResourceKey 0) StmMap.new
  newResources <- liftIO $ newIORef []
  newJoints <- liftIO $ newIORef []
  let childFree = do
        env <- ask
        liftIO $ do
          joints <- readIORef (threadJoints env)
          forM_ joints (\x -> takeMVar x `catchAll` const (return ()))
        clearResources env
  let realAction = action `finally` childFree
  runReaderT realAction $ ThreadEnv newCommon newResources newJoints

resourceFork ::
     (MonadIO m, MonadMask m)
  => (m () -> m ())
  -> AllocateT m ()
  -> AllocateT m ()
resourceFork fork (AllocateT act) = do
  myResourcesRef <- asks threadResources
  commonData <- asks common
  let mapData = resources commonData
  myResources <- liftIO $ readIORef myResourcesRef
  newResourcesRef <- liftIO $ newIORef myResources
  newJointsRef <- liftIO $ newIORef []
  mask_ $ do
    newEnv <-
      liftIO $ do
        atomically $
          let acquireFocus = Focus.adjust (\(c, d) -> (c + 1, d))
           in mapM_ (\key -> StmMap.focus acquireFocus key mapData) myResources
        return $ ThreadEnv commonData newResourcesRef newJointsRef
    myJointsRef <- asks threadJoints
    childJoint <- liftIO newEmptyMVar
    liftIO $ addElem myJointsRef childJoint
    let childFree = do
          env <- ask
          liftIO $ do
            joints <- readIORef (threadJoints env)
            forM_ joints (\x -> takeMVar x `catchAll` const (return ()))
          clearResources env
          liftIO $ putMVar childJoint ()
    let forkAction = act `finally` childFree
    lift $ fork $ runReaderT forkAction newEnv
