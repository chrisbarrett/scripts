module Itunes.Media
       (
         ImportTask(..)
       , Importable(..)
       , mediaFromPath
       )
       where

import qualified Data.ByteString.Lazy.Char8   as L8
import           Codec.Archive.Zip
import           Control.Applicative
import System.Directory
import           System.FilePath.Posix
import           Control.Monad


-- | Associates an item to import with a label for UI feedback.
data ImportTask = ImportTask
                  { taskName :: String
                  , runTask  :: IO () }

data Importable = MediaFile FilePath
                | ZipFile FilePath


-- | Filter the input files for importable items.
mediaFromPath :: FilePath -> IO [(FilePath, Importable)]
mediaFromPath p = do
  isDir <- doesDirectoryExist p
  isMedia <- isMediaFile p
  isZip <- isZipFile p
  case (isDir, isMedia, isZip) of
    (True, _, _) -> liftM concat $ getFilesInTree p >>= mapM mediaFromPath
    (_, True, _) -> return [ (p, MediaFile p) ]
    (_, _, True) -> return [ (p, ZipFile p) ]
    _            -> return []


-- | Walk the directory tree to find all files below a given path.
getFilesInTree :: FilePath -> IO [FilePath]
getFilesInTree d | takeFileName d `elem` [".", ".."] = return []
getFilesInTree d = do
  isDir <- doesDirectoryExist d
  isFile <- doesFileExist d
  case (isDir, isFile) of
    (True, _) -> concat <$> (getDirectoryContents d >>= mapM (getFilesInTree . (</>) d))
    (_, True) -> return [d]
    _         -> return []


-- | Create tasks to add the given media to the iTunes library.
importTasks :: FilePath -> Importable -> IO [ImportTask]

importTasks dest (MediaFile f) =
  return [ ImportTask { taskName = takeFileName f
                      , runTask = copyFile f $ dest </> takeFileName f } ]

importTasks dest (ZipFile f) = withArchive f $ do
  entries <- liftM (filter hasMediaExt) entryNames
  forM entries $ \x ->
    return ImportTask { taskName = x
                      , runTask = withArchive f $ extractFiles [x] dest }

-- | True if the given file can be imported by iTunes.
isMediaFile :: FilePath -> IO Bool
isMediaFile p = do
  exists <- doesFileExist p
  return $ exists && hasMediaExt p

-- | True if the given file has a media file extension.
hasMediaExt :: FilePath -> Bool
hasMediaExt p = takeExtension p `elem` [".m4a", ".m4v", ".mov", ".mp4", ".mp3", ".mpg", ".aac", ".aiff"]

-- | Read file header to test whether the given path points to a zip archive.
isZipFile :: FilePath -> IO Bool
isZipFile p = do
  isFile <- doesFileExist p
  if isFile
    then do header <- liftM (L8.unpack . L8.take 2) (L8.readFile p)
            return $ header == "PK"
    else return False
