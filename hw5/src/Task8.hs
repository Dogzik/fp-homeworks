{-# LANGUAGE Rank2Types #-}

module Task8
  ( changeExtention
  , getPath
  , listContentRecursive
  , listDirContentRecursively
  , listFilesRecursive
  , move
  , removeIfEmpty
  ) where

import           Data.Maybe      (isJust)
import           Lens.Micro      (SimpleFold, SimpleGetter, Traversal', failing,
                                  filtered, to, traversed, (%~), (&), (^.),
                                  (^..), (^?))
import           System.FilePath (addTrailingPathSeparator, replaceExtension,
                                  (</>))
import           Task6           (FS (..), childrenSubtrees, dirContents,
                                  dirName, fileName, fsName, subtree)

changeExtention :: String -> FS -> FS
changeExtention newExt dir =
  dir & dirContents . traversed . fileName %~ flip replaceExtension newExt

listFilesRecursive :: FS -> [FilePath]
listFilesRecursive fs = fs ^.. failing fileName listDir
  where
    listDir :: Traversal' FS FilePath
    listDir = dirContents . traversed . failing fileName listDir

listContentRecursive :: FS -> [FilePath]
listContentRecursive fs = fs ^.. subtree

listDirContentRecursively :: FS -> [FilePath]
listDirContentRecursively fs = fs ^.. childrenSubtrees

removeIfEmpty :: FilePath -> FS -> FS
removeIfEmpty dir fs = fs & dirContents %~ filterEmptyDirs
  where
    emptyDir e =
      let f1 = e ^? dirName . filtered (== dir)
          f2 = e ^? dirContents . filtered null
       in isJust f1 && isJust f2
    filterEmptyDirs = filter (not . emptyDir)

move :: FilePath -> SimpleFold FS FS
move path f fs@(Dir dirname elems) =
  let targets = elems ^.. traversed . filtered ((== path) . (^. fsName))
      updTargets = targets & traversed . fsName %~ (dirname </>)
   in (dirContents . traversed) f (fs {contents = updTargets})
move _ _ fs = pure fs

getPath :: SimpleGetter FS FilePath
getPath = to pathGetter
  where
    pathGetter Dir {name = dirname} = addTrailingPathSeparator dirname
    pathGetter (File filename)      = filename
