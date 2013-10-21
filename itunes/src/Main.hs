{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Main where

import           Control.Exception
import           Control.Monad
import           Itunes.Import
import           System.Directory   (canonicalizePath)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)


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
execute Help =
  putStrLn "itunes: Commands for working with iTunes" >> showUsage
execute Invalid =
  putStrLn "Invalid usage." >> showUsage >> exitFailure
execute (Unknown cmd) =
  putStrLn ("Unrecognised command: " ++ cmd) >> showUsage >> exitFailure
execute (Add args) =
  pathsFromArgs >>= addToItunes
  where
    pathsFromArgs = forM args $ \path ->
        canonicalizePath path `catch` (\(_::IOException) -> return path)
