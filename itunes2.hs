{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}
{-

 itunes

 AUTHOR: Chris Barrett <chris.d.barrett@me.com>
 LICENSE: BSD

 Copyright (c) 2013, Chris Barrett

 DESCRIPTION:
  Commands for working with iTunes from the command-line.

 CABAL DEPENDENCIES:
  ansi-wl-pprint
  zip-conduit = 0.2.2
  temporary = 1.1

-}

import           Codec.Archive.Zip
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8   as L8
import           Data.Char                    (toLower)
import           Data.Maybe                   (fromJust, isJust)
import           Prelude                      hiding (catch)
import           System.Directory
import           System.Environment           (getArgs)
import           System.Exit                  (exitFailure)
import           System.FilePath.Posix
import           Text.PrettyPrint.ANSI.Leijen (dullyellow, green, linebreak,
                                               putDoc, red, text, (<+>), (<>))

--------------------------------------------------------------------------------
-- Main program

-- | Enumerates the possible parsed values of the program arguments.
data Args = Add [FilePath] | Help | Invalid | Unknown String
          deriving Show

main :: IO ()
main = getArgs >>= execute . parseArgs
  where
    parseArgs ("add":xs) = if (not . null) xs then Add xs else Invalid
    parseArgs ("help":_) = Help
    parseArgs (x:_) = Unknown x
    parseArgs _     = Invalid

-- | Print program usage to stdout.
showUsage :: IO ()
showUsage =
  putStrLn $ unlines
  [ "Usage:"
  , "  add [items...]   Add files or folders to the iTunes library"
  , "  help             Show usage" ]

-- | Run the program as specified by the program arguments.
execute :: Args -> IO ()
execute Help          = putStrLn "itunes: Commands for working with iTunes" >> showUsage
execute Invalid       = putStrLn "Invalid usage." >> showUsage >> exitFailure
execute (Unknown cmd) = putStrLn ("Unrecognised command: " ++ cmd) >> showUsage >> exitFailure
execute (Add args)    = do
  itunesExists <- itunesImportFolder >>= doesDirectoryExist
  unless itunesExists $ putStrLn "Cannot find iTunes Media folder" >> exitFailure
  paths <- pathsFromArgs
  warnWhereNotExists paths
  xs <- liftM concat $ mapM mediaFromPath paths
  when (null xs) $ putStrLn "No media found." >> exitFailure

  let files = map fromJust $ filter isJust $ map fst xs
      media = map snd xs
  mapM_ importMedia media
  promptDeleteOriginals files

  where
    -- | Extract targets to be imported from program arguments.
    pathsFromArgs = forM args $ \path ->
        canonicalizePath path `catch` (\(_::IOException) -> return path)

    -- | Warn when trying to import items that do not exist on the filesystem.
    warnWhereNotExists paths = do
      notExists <- filterM (liftM not . fileOrDirectoryExists) paths
      unless (null notExists) $ do
        putDoc $ dullyellow (text "Warning: the following items do not exist:") <> linebreak
        mapM_ (\x -> putDoc $
                    dullyellow (text "  ? ")
                    <+> text (takeFileName x)
                    <> linebreak)
          notExists

    -- | Prompt the user whether to delete the original items after importing.
    promptDeleteOriginals :: [FilePath] -> IO ()
    promptDeleteOriginals xs = do
      let n = length xs
      putStrLn $ "Delete original " ++ pluralize n "item" ++ "? (y/n) [n] "
      shouldDelete <- getYesOrNo False
      when shouldDelete $ do
        forM_ xs $ \x -> do
          removeFile x
          putDoc $ red (text "  D ") <+> text x <> linebreak

        putStrLn $ "Deleted " ++ show n ++ " " ++ pluralize n "item" ++ "."

    -- | The path to the iTunes import folder.
    itunesImportFolder :: IO FilePath
    itunesImportFolder  = ( getHomeDirectory /> "Music" </> "iTunes" </> "iTunes Media" )
                          /> "Automatically Add to iTunes.localized"

    -- | Import each media item into iTunes.
    importMedia :: Importable -> IO ()
    importMedia x = do
      dest <- itunesImportFolder
      tasks <- importTasks dest x
      forM_ tasks $ \t -> do
        runTask t
        putDoc $ green (text "  A ") <+> text (taskName t)  <> linebreak


-- | Concatenate a monadic filepath with pure filepaths.
(/>) :: IO FilePath -> FilePath -> IO FilePath
io /> p = (</>) <$> io <*> pure p
infix 4 />

-- | Test whether the given file or directory exists.
fileOrDirectoryExists :: FilePath -> IO Bool
fileOrDirectoryExists x = or <$> sequence [doesDirectoryExist x, doesFileExist x]

type Count = Int
-- | Perform a naive string pluralisation.
pluralize :: Count -> String -> String
pluralize 1 str = str
pluralize _ str = str ++ "s"

type Default = Bool
-- | Prompt the user for a yes or no response, with a default answer.
getYesOrNo :: Default -> IO Bool
getYesOrNo deflt = do
  ch <- getChar
  case toLower ch of
    'y'  -> return True
    'n'  -> return False
    '\n' -> return deflt
    _    -> getYesOrNo deflt

--------------------------------------------------------------------------------
-- Filesystem utilities

-- | Filter the input files for importable items.
mediaFromPath :: FilePath -> IO [(FilePath, Importable)]
mediaFromPath p@(isMedia -> True) = return [ (p, MediaFile p) ]
mediaFromPath p = do
  dir <- doesDirectoryExist p
  zip <- isZipFile p
  return $ case (dir, zip) of
    (True, _) -> liftM concat $ getFilesInTree p >>= mapM mediaFromPath
    (_, True) -> return (p, ZipFile p)
    _         -> []


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
  entries <- liftM (filter isMedia) entryNames
  forM entries $ \x ->
    return ImportTask { taskName = x
                      , runTask = withArchive f $ extractFiles [x] dest
                      }

--------------------------------------------------------------------------------
-- Common types

-- | Associates an item to import with a label for UI feedback.
data ImportTask = ImportTask
                  { taskName :: String
                  , runTask  :: IO () }

data Importable = MediaFile FilePath
                | ZipFile FilePath

--------------------------------------------------------------------------------

-- | True if the given file can be imported by iTunes.
isMedia :: FilePath -> Bool
isMedia p = takeExtension p `elem` [".m4a", ".m4v", ".mov", ".mp4", ".mp3", ".mpg", ".aac", ".aiff"]

-- | Read file header to test whether the given path points to a zip archive.
isZipFile :: FilePath -> IO Bool
isZipFile p = do
  header <- liftM (L8.unpack . L8.take 2) (L8.readFile p)
  return $ header == "PK"
