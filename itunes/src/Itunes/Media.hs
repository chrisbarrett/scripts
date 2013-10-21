module Itunes.Media
       (
         ImportTask(..)
       , Importable(..)
       , importTasks
       , isMediaFile
       , isZipFile
       )
       where

import           Codec.Archive.Zip
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L8
import           System.Directory
import           System.FilePath.Posix


-- | Associates an item to import with a label for UI feedback.
data ImportTask = ImportTask
                  { taskName :: String
                  , runTask  :: IO () }

data Importable = MediaFile FilePath
                | ZipFile FilePath


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
hasMediaExt p =
  takeExtension p `elem` [".m4a", ".m4v", ".mov", ".mp4", ".mp3", ".mpg", ".aac", ".aiff"]

-- | Read file header to test whether the given path points to a zip archive.
isZipFile :: FilePath -> IO Bool
isZipFile p = do
  isFile <- doesFileExist p
  if isFile
    then do header <- liftM (L8.unpack . L8.take 2) (L8.readFile p)
            return $ header == "PK"
    else return False
