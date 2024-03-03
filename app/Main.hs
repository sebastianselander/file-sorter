{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Concurrent
import Control.Monad (forever)
import System.Directory (
    createDirectory,
    doesDirectoryExist,
    doesFileExist,
    renameFile, canonicalizePath,
 )
import System.Environment
import System.Exit (exitFailure)
import System.FSNotify
import System.FilePath
import System.IO (hFlush, stdout)
import Text.Printf (printf)

shouldAct :: Event -> Bool
shouldAct (Added{}) = True
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

-- Returns the target directory and target path in a tuple
findTargetPath :: FilePath -> Maybe (FilePath, FilePath)
findTargetPath path = case takeExtension path of
    "" -> Nothing
    _ : extension ->
        let targetDir = takeDirectory path </> extension
         in Just (targetDir, targetDir </> takeFileName path)

act :: Event -> IO ()
act (Added oldpath _time IsFile) = do
    putStrLn ""
    putStrLn $ "Path: " <> oldpath
    case findTargetPath oldpath of
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
act _ = return ()

main :: IO ()
main = do
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
            printf "Incorrect amount of arguments '%d'\nExpected one argument to a directory\n" (length _else) 
            hFlush stdout
            exitFailure
    canonical <- canonicalizePath path
    putStrLn $ "Watching " <> canonical
    mgr <- startManager
    _ <- watchDir mgr canonical shouldAct act
    _ <- forever $ threadDelay 1_000_000
    stopManager mgr
    putStrLn "Stopping..."
