{-

 itunes

 AUTHOR: Chris Barrett <chris.d.barrett@me.com>
 LICENSE: BSD

 Copyright (c) 2013, Chris Barrett

 DESCRIPTION:
  Commands for working with iTunes from the command-line.

 CABAL DEPENDENCIES:
  ansi-wl-pprint

-}

{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Char                    (toLower)
import           Prelude                      hiding (catch)
import           System.Directory
import           System.Environment           (getArgs)
import           System.Exit                  (exitFailure)
import           System.FilePath.Posix
import           Text.PrettyPrint.ANSI.Leijen (dullyellow, green, linebreak,
                                               putDoc, red, text, (<+>), (<>))

--- Enumerates the possible parsed values of the program arguments.
data Args = Add [FilePath] | Help | Invalid | Unknown String
          deriving Show

--- Enumerates different filetypes that need to be handled when searching for media.
data FileType = Media FilePath | Archive FilePath | Unsupported

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
  media <- mapM mediaFromPath paths
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
        mapM_ (\x -> putDoc $ dullyellow (text "  ? ") <+> text x <> linebreak) notExists

    promptDeleteOriginals files = do
      let n = length files
      putStrLn $ "Delete original " ++ pluralize n "item" ++ "? (y/n) [n] "
      shouldDelete <- getYesOrNo False
      when shouldDelete $ do
        forM_ files $ \x -> do
          removeFile x
          putDoc $ red (text "  D ") <+> text x <> linebreak

        putStrLn $ "Deleted " ++ show n ++ " " ++ pluralize n "item" ++ "."

--- Concatenate a monadic filepath with pure filepaths.
(/>) :: Monad m => m FilePath -> FilePath -> m FilePath
io /> p = (</>) <$> io <*> pure p
infix 4 />

--- The path to the iTunes library in the user's home folder.
itunesMedia :: IO FilePath
itunesMedia = getHomeDirectory /> "Music" </> "iTunes" </> "iTunes Media"

--- Copy the given file to the iTunes library.
addToItunes :: FilePath -> IO ()
addToItunes file = do
  dest <- itunesMedia /> "Automatically Add to iTunes.localized" </> takeFileName file
  copyFile file dest
  putDoc $ green (text "  A ") <+> text (takeFileName file) <> linebreak

--- Valid media extensions
mediaExtensions :: [String]
mediaExtensions = [".m4a", ".m4v", ".mov", ".mp4", ".mp3", ".mpg", ".aac", ".aiff"]

--- Search for media files under the given filepath.
mediaFromPath :: FilePath -> IO [FilePath]
mediaFromPath path = do
  exists <- fileOrDirectoryExists path
  if exists
    then getFilesInTree path >>= mapM categoriseType >>= mapM selectMedia
    else return []

--- Inspect the given file and determine whether it is an actionable type.
categoriseType :: FilePath -> IO FileType
categoriseType = undefined

--- Map the given file to its media items. Search archives for media.
selectMedia :: FileType -> IO [FilePath]
selectMedia = undefined

--- Walk the directory tree to find all files below a given path.
getFilesInTree :: FilePath -> IO [FilePath]
getFilesInTree d | takeFileName d `elem` [".", ".."] = return []
getFilesInTree d = do
  isDir <- doesDirectoryExist d
  isFile <- doesFileExist d
  case (isDir, isFile) of
    (True, _) -> concat <$> (getDirectoryContents d >>= mapM (getFilesInTree . (</>) d))
    (_, True) -> return [d]
    otherwise -> return []

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
