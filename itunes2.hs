{-# LANGUAGE ScopedTypeVariables #-}
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
import           Prelude                      hiding (catch)
import           System.Directory
import           System.Environment           (getArgs)
import           System.Exit                  (exitFailure)
import           System.FilePath.Posix
import           System.IO.Temp
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
  media <- liftM concat $ mapM mediaFromPath paths
  when (null media) $ putStrLn "No media found." >> exitFailure
  importMedia media
  promptDeleteOriginals media

  where
    -- | Extract targets to be imported from program arguments.
    pathsFromArgs = do
      forM args $ \path ->
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
    promptDeleteOriginals :: (Deleteable a, Describable a) => [a] -> IO ()
    promptDeleteOriginals xs = do
      let n = length xs
      putStrLn $ "Delete original " ++ pluralize n "item" ++ "? (y/n) [n] "
      shouldDelete <- getYesOrNo False
      when shouldDelete $ do
        forM_ xs $ \x -> do
          delete x
          putDoc $ red (text "  D ") <+> text (describe x) <> linebreak

        putStrLn $ "Deleted " ++ show n ++ " " ++ pluralize n "item" ++ "."

    -- | The path to the iTunes import folder.
    itunesImportFolder :: IO FilePath
    itunesImportFolder  = ( getHomeDirectory /> "Music" </> "iTunes" </> "iTunes Media" )
                          /> "Automatically Add to iTunes.localized"

    -- | Import each media item into iTumes.
    importMedia :: (Importable a, Describable a) => [a] -> IO ()
    importMedia = mapM_ $ \x -> do
      dest <- itunesImportFolder
      runImport dest x
      putDoc $ green (text "  A ") <+> text (describe x)  <> linebreak


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
mediaFromPath :: (Importable a, Deleteable a) => FilePath -> IO [a]
mediaFromPath p = undefined

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

--------------------------------------------------------------------------------
-- Common types

-- | Associates an item to import with a label for UI feedback.
data ImportTask = ImportTask
                  { itemName :: String
                  , action   :: IO () }

-- | Represents things that can be imported into iTunes.
class Importable a where
  -- | Add the given media to the iTunes library.
  getImports :: FilePath -> a -> IO [ImportTask]

-- | Represents things that can be deleted.
class Deleteable a where
  -- | Delete the given item.
  delete :: a -> IO ()

--------------------------------------------------------------------------------
-- Media files

newtype MediaFile = MediaFile FilePath

-- | True if the given file can be imported by iTunes.
isMedia :: FilePath -> Bool
isMedia p = takeExtension p `elem` [".m4a", ".m4v", ".mov", ".mp4", ".mp3", ".mpg", ".aac", ".aiff"]

-- | Construct a MediaFile instance from the given file if it is importable.
asMediaFile :: FilePath -> IO (Maybe MediaFile)
asMediaFile p@(isMedia -> True) = return $ Just $ MediaFile p
asMediaFile _ = return Nothing

instance Importable MediaFile where
  getImports dest (MediaFile f) =
    return { itemName = takeFileName f
           , action = copyFile f $ dest </> takeFileName f
           }

instance Deleteable MediaFile where
  delete (MediaFile f) = removeFile f

--------------------------------------------------------------------------------
-- Zip files

newtype Zip = Zip FilePath

-- | Read file header to test whether the given path points to a zip archive.
isZipFile :: FilePath -> IO Bool
isZipFile p = do
  header <- liftM (L8.unpack . L8.take 2) (L8.readFile p)
  return $ header == "PK"

-- | Construct a Zip instance from the given file if it is a zip file.
asZipFile :: FilePath -> IO (Maybe Zip)
asZipFile p = do
  isZip <- isZipFile p
  return $ case isZip of
    True  -> Just $ Zip p
    False -> Nothing

instance Importable Zip where
  getImports dest z@(Zip f) = withArchive f $ do
      liftM (filter isMedia) entryNames >>= mapM $ \x ->
        return { itemName = x
               , action = \() -> withArchive f $ extractFiles [x] dest
               }

instance Deleteable Zip where
  delete (Zip z) = removeFile z
