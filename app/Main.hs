{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, void, when, zipWithM_)
import Data.Map (Map)
import Data.Map qualified as Map
import Parser (parseConfig)
import System.Directory (
    canonicalizePath,
    createDirectory,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    renameFile,
 )
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FSNotify (
    Event (..),
    EventIsDirectory (..),
    startManager,
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

isFile :: Event -> Bool
isFile (Added _ _ IsFile) = True
isFile _ = False

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
        let ext = Map.findWithDefault extension extension dictionary
            targetDir = takeDirectory path </> ext
            fileName = takeFileName path
         in case ext of 
            "ignore" -> Nothing
            _        -> Just (targetDir, targetDir </> fileName)

-- On a file being added to the directory,
-- move that file to the appropriate directory
act :: Map FilePath FilePath -> Event -> IO ()
act dictionary (Added path _ IsFile) = void $ moveFile dictionary path
act _ _ = return ()

moveFile :: Map FilePath FilePath -> FilePath -> IO (Maybe String)
moveFile dictionary path = case findTargetPath dictionary path of
    Just (targetDir, targetPath) -> do
        doesDirectoryExist targetDir >>= \case
            False -> do
                createDirectory targetDir
                renameFile path targetPath
                pure (pure targetPath)
            True -> do
                newTargetPath <- findNewTargetPath targetPath
                renameFile path newTargetPath
                pure (pure newTargetPath)
    Nothing -> pure (fail "File should not be moved")

directoryExist :: FilePath -> IO ()
directoryExist path = do
    doesDirectoryExist path >>= \case
        True -> pure ()
        False -> do
            putStrLn $ "'" <> path <> "' does not exist or is not a directory"
            exitFailure

sortDirectory :: Map FilePath FilePath -> FilePath -> IO ()
sortDirectory dict path = do
    directoryExist path
    files <- map (path </>) <$> listDirectory path
    filesMoved <- mapM (moveFile dict) files
    zipWithM_ showIfMoved files filesMoved
  where
    showIfMoved :: String -> Maybe String -> IO ()
    showIfMoved from (Just to) =
        putStrLn ("Moved '" <> from <> "' to '" <> to <> "'")
    showIfMoved _ Nothing = pure ()

main :: IO ()
main = do
    when
        (os /= "linux")
        (putStrLn ("Bad operating system: " <> os) >> exitFailure)
    args <- getArgs
    (path, dictionary) <- case args of
        ["--move", path] -> do
            sortDirectory mempty path
            putStrLn ("Sorted '" <> path <> "'")
            exitSuccess
        [path, "--move"] -> do
            sortDirectory mempty path
            putStrLn ("Sorted '" <> path <> "'")
            exitSuccess
        [path] -> directoryExist path >> pure (path, mempty)
        [path, configuration] -> do
            directoryExist path
            dict <- parseConfig configuration
            pure (path, dict)
        _else -> do
            putStrLn "Can't figure out what to do"
            exitFailure
    canonical <- canonicalizePath path
    mgr <- startManager
    void $ watchDir mgr canonical isFile (act dictionary)
    void $ forever (threadDelay 1_000_000)
