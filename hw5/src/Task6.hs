module Task6
  ( FS(..)
  , scanDirectory
  ) where

import           Control.Exception (SomeException, catch)
import           Control.Monad     (filterM, forM)
import           System.Directory  (doesDirectoryExist, doesFileExist,
                                    listDirectory, pathIsSymbolicLink)

import           System.FilePath   (splitDirectories, takeFileName, (</>))

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

listHandle :: SomeException -> IO [FilePath]
listHandle = const $ return []

getDirName :: FilePath -> FilePath
getDirName path = last $ splitDirectories path

scanDirectory :: FilePath -> IO FS
scanDirectory path = Dir (getDirName path) <$> getContent path
  where
    getContent p = do
      elems <- listDirectory p
      let content = map (p </>) elems
      goodContent <- filterM isGood content
      forM goodContent $ \child -> do
        file <- isFile child
        if file
          then return $ File $ takeFileName child
          else Dir (getDirName child) <$> getContent child
