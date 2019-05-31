module Task8
  ( changeExtention
  , listContentRecursive
  , listFilesRecursive
  , removeIfEmpty
  ) where

import           Data.Maybe      (isJust)
import           Lens.Micro      (Traversal', failing, filtered, traversed,
                                  (%~), (&), (^.), (^..), (^?))
import           System.FilePath (replaceExtension)
import           Task6           (FS, dirContents, dirName, fileName, fsName)

changeExtention :: String -> FS -> FS
changeExtention newExt dir =
  dir & dirContents . traversed . fileName %~ flip replaceExtension newExt

listFilesRecursive :: FS -> [FilePath]
listFilesRecursive fs = fs ^.. failing fileName listDir
  where
    listDir :: Traversal' FS FilePath
    listDir = dirContents . traversed . failing fileName listDir

listContentRecursive :: FS -> [FilePath]
listContentRecursive fs =
  let curName = fs ^. fsName
      children = concatMap listContentRecursive (fs ^.. dirContents . traversed)
   in curName : children

removeIfEmpty :: FilePath -> FS -> FS
removeIfEmpty dir fs = fs & dirContents %~ filterEmptyDirs
  where
    emptyDir e =
      let f1 = e ^? dirName . filtered (== dir)
          f2 = e ^? dirContents . filtered null
       in isJust f1 && isJust f2
    filterEmptyDirs = filter (not . emptyDir)
