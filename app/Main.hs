{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, void, when)
import Data.Map (Map)
import Data.Map qualified as Map
import System.Directory (
    canonicalizePath,
    createDirectory,
    doesDirectoryExist,
    doesFileExist,
    renameFile,
 )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FSNotify (
    Event (..),
    EventIsDirectory (..),
    startManager,
    stopManager,
    watchDir,
 )
import System.FilePath (
    dropExtension,
    takeDirectory,
    takeExtension,
    takeFileName,
    (</>),
 )
import System.IO (hFlush, stdout)
import System.Info (os)
import Text.Printf (printf)

shouldAct :: Event -> Bool
shouldAct Added{} = True
shouldAct _ = False

-- Finds non-overlappig appropriate name.
-- Does nothing if there are no collisions
findNewTargetPath :: FilePath -> IO FilePath
findNewTargetPath path = go Nothing
  where
    go :: Maybe Int -> IO FilePath
    go Nothing =
        doesFileExist path >>= \case
            True -> go (Just 1)
            False -> pure path
    go (Just n) = do
        let basename = dropExtension path
        let extension = takeExtension path
        let newpath = basename <> "(" <> show n <> ")" <> extension
        doesFileExist newpath >>= \case
            False -> pure newpath
            True -> go (Just (n + 1))

-- | Find the target path based on the file extension.
-- If the extension is mapped to something in the dictionary choose that one,
-- otherwise the extension is chosen as the target directory.
findTargetPath ::
    Map FilePath FilePath ->
    FilePath ->
    Maybe (FilePath, FilePath)
findTargetPath dictionary path = case takeExtension path of
    "" -> Nothing
    _ : extension ->
        let targetDir =
                takeDirectory path
                    </> Map.findWithDefault
                        extension
                        extension
                        dictionary
            fileName = takeFileName path
         in Just (targetDir, targetDir </> fileName)

-- On a file being added to the directory,
-- move that file to the appropriate directory 
act :: Map FilePath FilePath -> Event -> IO ()
act dictionary (Added oldpath _time IsFile) = do
    putStrLn ""
    putStrLn $ "Path: " <> oldpath
    case findTargetPath dictionary oldpath of
        Just (targetDir, targetPath) -> do
            print (targetDir, targetPath)
            doesDirectoryExist targetDir >>= \case
                False -> do
                    createDirectory targetDir
                    renameFile oldpath targetPath
                True -> do
                    newTargetPath <- findNewTargetPath targetPath
                    renameFile oldpath newTargetPath
        Nothing -> return ()
act _ _ = return ()

main :: IO ()
main = do
    when
        (os /= "linux")
        (putStrLn ("Bad operating system: " <> os) >> exitFailure)
    args <- getArgs
    path <- case args of
        (x : _) ->
            doesDirectoryExist x >>= \case
                True -> pure x
                False -> do
                    printf "'%s' is not a directory\n" x
                    hFlush stdout
                    exitFailure
        _else -> do
            putStrLn "Incorrect amount of arguments"
            putStrLn "Expected one argument to a directory"
            exitFailure
    -- TODO: Let users customize mappings by some config file
    let dictionary = mempty
    canonical <- canonicalizePath path
    putStrLn $ "Watching " <> canonical
    mgr <- startManager
    void $ watchDir mgr canonical shouldAct (act dictionary)
    void $ forever (threadDelay 1_000_000)
    stopManager mgr
    putStrLn "Stopping..."
