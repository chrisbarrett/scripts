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

--- Enumerates the possible parsed values of the program arguments.
data Args = Add [FilePath] | Help | Invalid | Unknown String
          deriving Show

--- Enumerates the different media form that will be added to iTunes.
data MediaType = File FilePath
               | Stream L8.ByteString Name (Maybe ArchivePath)
type Name = String
type ArchivePath = FilePath

--- Enumerates different filetypes that need to be handled when searching for media.
data FileType = Media MediaType
              | Zip ArchivePath
              | Unsupported

main :: IO ()
main = getArgs >>= execute . parseArgs
  where
    parseArgs ("add":xs) = if (not . null) xs then Add xs else Invalid
    parseArgs ("help":_) = Help
    parseArgs (x:_) = Unknown x
    parseArgs _     = Invalid

--- Print program usage to stdout.
showUsage :: IO ()
showUsage =
  putStrLn $ unlines
  [ "Usage:"
  , "  add [items...]   Add files or folders to the iTunes library"
  , "  help             Show usage" ]

--- Run the program as specified by the program arguments.
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
    deleteMedia (File path) = do
      removeFile path
      putDoc $ red (text "  D ") <+> text path <> linebreak
    deleteMedia (Stream _ _ (Just archive)) = undefined

--- Concatenate a monadic filepath with pure filepaths.
(/>) :: IO FilePath -> FilePath -> IO FilePath
io /> p = (</>) <$> io <*> pure p
infix 4 />

--- The path to the iTunes library in the user's home folder.
itunesMedia :: IO FilePath
itunesMedia = getHomeDirectory /> "Music" </> "iTunes" </> "iTunes Media"

--- Add the given media to the iTunes library.
addToItunes :: MediaType -> IO ()
addToItunes (File file) = do
  dest <- itunesMedia /> "Automatically Add to iTunes.localized" </> takeFileName file
  copyFile file dest
  putDoc $ green (text "  A ") <+> text (takeFileName file) <> linebreak
addToItunes (Stream bs name _) = undefined

isMedia :: FilePath -> Bool
isMedia p = p `elem` [".m4a", ".m4v", ".mov", ".mp4", ".mp3", ".mpg", ".aac", ".aiff"]



asZipFile :: FilePath -> IO Bool
asZipFile p = do
  -- Read magic string from file header to determine type.
  bs <- liftM (L8.unpack . L8.take 2) (L8.readFile p)
  return $ case bs of
    "PK" -> Just $ Zip p
    _    -> Nothing

--- Walk the directory tree to find all files below a given path.
getFilesInTree :: FilePath -> IO [FilePath]
getFilesInTree d | takeFileName d `elem` [".", ".."] = return []
getFilesInTree d = do
  isDir <- doesDirectoryExist d
  isFile <- doesFileExist d
  case (isDir, isFile) of
    (True, _) -> concat <$> (getDirectoryContents d >>= mapM (getFilesInTree . (</>) d))
    (_, True) -> return [d]
    _         -> return []

--- Test whether the given file or directory exists.
fileOrDirectoryExists :: FilePath -> IO Bool
fileOrDirectoryExists x = or <$> sequence [doesDirectoryExist x, doesFileExist x]

type Count = Int
--- Perform a naive string pluralisation.
pluralize :: Count -> String -> String
pluralize 1 str = str
pluralize _ str = str ++ "s"

type Default = Bool
--- Prompt the user for a yes or no response, with a default answer.
getYesOrNo :: Default -> IO Bool
getYesOrNo deflt = do
  ch <- getChar
  case toLower ch of
    'y'  -> return True
    'n'  -> return False
    '\n' -> return deflt
    _    -> getYesOrNo deflt