module Itunes.Import
       (
         addToItunes
       )
       where
import           Control.Monad
import           Control.Applicative
import           Control.Exception
import           Data.Char                    (toLower)
import           Prelude                      hiding (catch)
import           System.Directory

import           Text.PrettyPrint.ANSI.Leijen (dullyellow, green, linebreak,
                                               putDoc, red, text, (<+>), (<>))
addToItunes :: [FilePath] -> IO ()
addToItunes paths = do
  itunesExists <- itunesImportFolder >>= doesDirectoryExist
  unless itunesExists $ putStrLn "Cannot find iTunes Media folder" >> exitFailure
  warnWhereNotExists paths
  xs <- liftM concat $ mapM mediaFromPath paths
  when (null xs) $ putStrLn "No media found." >> exitFailure

  let files = map fst xs
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


-- | Concatenate a monadic filepath with a pure filepath.
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
