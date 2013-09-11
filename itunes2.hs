{-# LANGUAGE ScopedTypeVariables #-}
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
    promptDeleteOriginals :: Deleteable a => [a] -> IO ()
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
    importMedia :: Importable a => [a] -> IO ()
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

-- | Represents things that can be imported into iTunes.
class Importable a where
  -- | Add the given media to the iTunes library.
  runImport :: FilePath -> a -> IO ()
  -- | String representation of the given item, for feedback in the UI.
  describe :: a -> String

-- | Represents things that can be deleted.
class Deleteable a where
  -- | Delete the given item.
  delete :: a -> IO ()

--------------------------------------------------------------------------------
-- Media files

newtype MediaFile = MediaFile FilePath

instance Importable MediaFile where
  describe (MediaFile f) = takeFileName f
  runImport dest (MediaFile f) = do
    copyFile f $ dest </> takeFileName f

-- | True if the given file can be imported by iTunes.
isMedia :: FilePath -> Bool
isMedia p = p `elem` [".m4a", ".m4v", ".mov", ".mp4", ".mp3", ".mpg", ".aac", ".aiff"]

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
  describe (Zip f) = show f
  runImport z = undefined

instance Deleteable Zip where
  delete (Zip f) = removeFile f

--------------------------------------------------------------------------------

-- | Filter the input files for importable items.
mediaFromPath :: Importable a => FilePath -> IO [a]
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