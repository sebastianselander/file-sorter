{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, void, when)
import Data.Map (Map)
import Data.Map qualified as Map
import Parser (parseConfig)
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
import System.Info (os)

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

{- | Find the target path based on the file extension.
If the extension is mapped to something in the dictionary choose that one,
otherwise the extension is chosen as the target directory.
-}
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
            doesDirectoryExist targetDir >>= \case
                False -> do
                    createDirectory targetDir
                    renameFile oldpath targetPath
                True -> do
                    newTargetPath <- findNewTargetPath targetPath
                    renameFile oldpath newTargetPath
        Nothing -> return ()
act _ _ = return ()

directoryExist :: FilePath -> IO ()
directoryExist path = do
    doesDirectoryExist path >>= \case
        True -> pure ()
        False -> do
            putStrLn $ "'" <> path <> "' does not exist or is not a directory"
            exitFailure

main :: IO ()
main = do
    when
        (os /= "linux")
        (putStrLn ("Bad operating system: " <> os) >> exitFailure)
    args <- getArgs
    (path, dictionary) <- case args of
        [path] -> directoryExist path >> pure (path, mempty)
        [path, configuration] -> do
            directoryExist path
            dict <- parseConfig configuration
            pure (path, dict)
        _else -> do
            putStrLn "Incorrect amount of arguments"
            putStrLn "Expected:\n    file-sorter <PATH_TO_DIR> (optional: <PATH_TO_CONFIG>)"
            exitFailure
    canonical <- canonicalizePath path
    putStrLn $ "Watching " <> canonical
    mgr <- startManager
    void $ watchDir mgr canonical shouldAct (act dictionary)
    void $ forever (threadDelay 1_000_000)
    stopManager mgr
    putStrLn "Stopping..."
