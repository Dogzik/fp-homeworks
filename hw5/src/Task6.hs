module Task6
  ( FS(..)
  , childrenSubtrees
  , dirContents
  , dirName
  , fileName
  , fsName
  , scanDirectory
  , subtree
  ) where

import           Control.Applicative (liftA2)
import           Control.Exception   (SomeException, catch)
import           Control.Monad       (filterM, forM)
import           System.Directory    (doesDirectoryExist, doesFileExist,
                                      listDirectory, pathIsSymbolicLink)

import           Lens.Micro          (Lens', Traversal', lens)
import           System.FilePath     (splitDirectories, takeFileName, (</>))
import           System.IO.Error     (ioError, userError)

data FS
  = Dir
      { name     :: FilePath
      , contents :: [FS]
      }
  | File
      { name :: FilePath
      }
  deriving (Show)

simpleHandle :: SomeException -> IO Bool
simpleHandle = const $ return False

isDir :: FilePath -> IO Bool
isDir p = do
  dir <- doesDirectoryExist p `catch` simpleHandle
  symlink <- pathIsSymbolicLink p `catch` simpleHandle
  return $! dir && not symlink

isFile :: FilePath -> IO Bool
isFile p = do
  file <- doesFileExist p `catch` simpleHandle
  symlink <- pathIsSymbolicLink p `catch` simpleHandle
  return $! file && not symlink

isGood :: FilePath -> IO Bool
isGood p = do
  dir <- isDir p
  file <- isFile p
  return $! dir || file

simpleListingHandle :: SomeException -> IO [FilePath]
simpleListingHandle _ = pure []

getDirName :: FilePath -> FilePath
getDirName path = last $ splitDirectories path

scanDirectory :: FilePath -> IO FS
scanDirectory path = do
  dir <- isDir path
  if not dir
    then ioError $ userError "No such directory"
    else Dir (getDirName path) <$> getContent path
  where
    getContent p = do
      elems <- listDirectory p `catch` simpleListingHandle
      let content = map (p </>) elems
      goodContent <- filterM isGood content
      forM goodContent $ \child -> do
        file <- isFile child
        if file
          then return $ File $ takeFileName child
          else Dir (getDirName child) <$> getContent child

fsName :: Lens' FS FilePath
fsName = lens getName setName
  where
    getName = name
    setName tree newName = tree {name = newName}

dirName :: Traversal' FS FilePath
dirName f fs@Dir {name = x} = (\newName -> fs {name = newName}) <$> f x
dirName _ fs                = pure fs

fileName :: Traversal' FS FilePath
fileName f (File x) = File <$> f x
fileName _ fs       = pure fs

dirContents :: Traversal' FS [FS]
dirContents f fs@Dir {contents = x} =
  (\newContents -> fs {contents = newContents}) <$> f x
dirContents _ fs = pure fs

subtree :: Traversal' FS FilePath
subtree f (Dir dirname dircontent) =
  liftA2 Dir (f dirname) (traverse (subtree f) dircontent)
subtree f (File filename) = File <$> f filename

childrenSubtrees :: Traversal' FS FilePath
childrenSubtrees f (Dir dirname dircontent) =
  Dir dirname <$> traverse (subtree f) dircontent
childrenSubtrees _ fs = pure fs
