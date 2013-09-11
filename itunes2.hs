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
  itunesExists <- itunesMedia >>= doesDirectoryExist
  unless itunesExists $ putStrLn "Cannot find iTunes Media folder" >> exitFailure
  paths <- pathsFromArgs
  warnWhereNotExists paths
  media <- liftM concat $ mapM mediaFromPath paths
  when (null media) $ putStrLn "No media found." >> exitFailure
  mapM_ addToItunes media
  promptDeleteOriginals media

  where
    pathsFromArgs =
      forM args $ \path ->
        canonicalizePath path `catch` (\(_::IOException) -> return path)

    warnWhereNotExists paths = do
      notExists <- filterM (liftM not . fileOrDirectoryExists) paths
      unless (null notExists) $ do
        putDoc $ dullyellow (text "Warning: the following items do not exist:") <> linebreak
        mapM_ (\x -> putDoc $
                    dullyellow (text "  ? ")
                    <+> text (takeFileName x)
                    <> linebreak)
          notExists

    promptDeleteOriginals :: [MediaType] -> IO ()
    promptDeleteOriginals files = do
      let n = length files
      putStrLn $ "Delete original " ++ pluralize n "item" ++ "? (y/n) [n] "
      shouldDelete <- getYesOrNo False
      when shouldDelete $ do
        forM_ files deleteMedia
        putStrLn $ "Deleted " ++ show n ++ " " ++ pluralize n "item" ++ "."

    deleteMedia :: MediaType -> IO ()
    deleteMedia (Stream _ _ (Just archive)) = rm archive
    deleteMedia (File path)                 = rm path

    rm :: FilePath -> IO ()
    rm p = do
      exists <- doesFileExist p
      when exists $ do
        removeFile p
        putDoc $ red (text "  D ") <+> text p <> linebreak


-- | Concatenate a monadic filepath with pure filepaths.
(/>) :: IO FilePath -> FilePath -> IO FilePath
io /> p = (</>) <$> io <*> pure p
infix 4 />

-- | The path to the iTunes library in the user's home folder.
itunesMedia :: IO FilePath
itunesMedia = getHomeDirectory /> "Music" </> "iTunes" </> "iTunes Media"

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
class Importable m where
  -- | Add the given media to the iTunes library.
  addToItunes :: m -> IO ()
  -- | String representation of the given item, for feedback in the UI.
  describe :: m -> String

--------------------------------------------------------------------------------
-- Media files

newtype MediaFile = MediaFile FilePath

instance Importable MediaFile where
  describe = show
  addToItunes file = do
    dest <- itunesMedia /> "Automatically Add to iTunes.localized" </> takeFileName file
    copyFile file dest
    putDoc $ green (text "  A ") <+> text (takeFileName file) <> linebreak

--------------------------------------------------------------------------------
-- Zip files

newtype Zip = Zip FilePath

-- | Construct a Zip instance from the given file if it is a zip file.
asZipFile :: FilePath -> IO (Maybe Zip)
asZipFile p = do
  -- Read magic string from file header to determine type.
  bs <- liftM (L8.unpack . L8.take 2) (L8.readFile p)
  return $ case bs of
    "PK" -> Just $ Zip p
    _    -> Nothing

instance Importable Zip where
  describe = show
  addToItunes



-- | True if the given file can be imported by iTunes.
isMedia :: FilePath -> Bool
isMedia p = p `elem` [".m4a", ".m4v", ".mov", ".mp4", ".mp3", ".mpg", ".aac", ".aiff"]


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
