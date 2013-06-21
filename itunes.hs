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

data Args = Add [FilePath] | Help | Invalid | Unknown String
          deriving Show

main :: IO ()
main = getArgs >>= execute . parseArgs
  where
    parseArgs ("add":xs) = if (not . null) xs then Add xs else Invalid
    parseArgs ("help":_) = Help
    parseArgs (x:_) = Unknown x
    parseArgs _     = Invalid

showUsage :: IO ()
showUsage =
  putStrLn $ unlines
  [ "Usage:"
  , "  add [items...]   Add files or folders to the iTunes library"
  , "  help             Show usage" ]

execute :: Args -> IO ()
execute Help          = putStrLn "itunes: Commands for working with iTunes" >> showUsage
execute Invalid       = putStrLn "Invalid usage." >> showUsage >> exitFailure
execute (Unknown cmd) = putStrLn ("Unrecognised command: " ++ cmd) >> showUsage >> exitFailure
execute (Add args)    = do
  itunesExists <- itunesMedia >>= doesDirectoryExist
  unless itunesExists $ putStrLn "Cannot find iTunes Media folder" >> exitFailure
  paths <- pathsFromArgs
  warnWhereNotExists paths
  media <- mediaFromPaths paths
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

    mediaFromPaths paths =
      filter isMedia . concat <$>
      (filterM fileOrDirectoryExists paths >>= mapM getFilesInTree)

    isMedia file =
      takeExtension file `elem` [".m4a", ".m4v", ".mov", ".mp4", ".mp3", ".aac", ".aiff"]

    promptDeleteOriginals files = do
      let n = length files
      putStrLn $ "Delete original " ++ pluralize "item" n ++ "? (y/n) [n] "
      shouldDelete <- getYesOrNo False
      when shouldDelete $ do
        forM_ files $ \x -> do
          removeFile x
          putDoc $ red (text "  D ") <+> text x <> linebreak

        putStrLn $ "Deleted " ++ show n ++ " " ++ pluralize "item" n ++"."

(<//>) :: IO FilePath -> FilePath -> IO FilePath
io <//> p = (</>) <$> io <*> pure p

itunesMedia :: IO FilePath
itunesMedia = getHomeDirectory <//> ("Music" </> "iTunes" </> "iTunes Media")

addToItunes :: FilePath -> IO ()
addToItunes file = do
  dest <- itunesMedia <//> ("Automatically Add to iTunes.localized" </> takeFileName file)
  copyFile file dest
  putDoc $ green (text "  A ") <+> text (takeFileName file) <> linebreak

getFilesInTree :: FilePath -> IO [FilePath]
getFilesInTree d | takeFileName d `elem` [".", ".."] = return []
getFilesInTree d = do
  isDir <- doesDirectoryExist d
  if isDir
    then concat <$> (getDirectoryContents d >>= mapM (getFilesInTree . (</>) d))
    else return [d]

fileOrDirectoryExists :: FilePath -> IO Bool
fileOrDirectoryExists x = or <$> sequence [doesDirectoryExist x, doesFileExist x]

type Count = Int
pluralize :: String -> Count -> String
pluralize str 1 = str
pluralize str _ = str ++ "s"

type Default = Bool
getYesOrNo :: Default -> IO Bool
getYesOrNo deflt = do
  ch <- getChar
  case toLower ch of
    'y'  -> return True
    'n'  -> return False
    '\n' -> return deflt
    _    -> getYesOrNo deflt
