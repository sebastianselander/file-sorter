{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, void, when, zipWithM_)
import Data.Map (Map)
import Data.Map qualified as Map
import Options (Options (..), options)
import Parser (Extension (..), parseConfig)
import System.Directory (
    canonicalizePath,
    createDirectory,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    renameFile,
 )
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

todo :: a
todo = error "TODO: Not yet implemented"

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
            extension = takeExtension path
            newpath = basename <> paren (show n) <> extension
        doesFileExist newpath >>= \case
            False -> pure newpath
            True -> go (Just (n + 1))
      where
        paren :: String -> String
        paren s = '(' : s <> ")"

type Directory = String
type Path = String

data Target = TargetIgnore | Move !Directory !Path

{- | Find the target path based on the file extension.
If the extension is mapped to something in the dictionary choose that one,
otherwise the extension is chosen as the target directory.
-}
findTargetPath :: Map FilePath Extension -> FilePath -> Target
findTargetPath dictionary path = case takeExtension path of
    "" -> TargetIgnore
    _ : extension ->
        case Map.findWithDefault (Ext extension) extension dictionary of
            ExtIgnore -> TargetIgnore
            (Ext e) ->
                let targetDir = takeDirectory path </> e
                    fileName = takeFileName path
                 in Move targetDir (targetDir </> fileName)

-- On a file being added to the directory,
-- move that file to the appropriate directory
act :: Map FilePath Extension -> Event -> IO ()
act dictionary (Added path _ IsFile) = void $ moveFile dictionary path
act _ _ = return ()

moveFile :: Map FilePath Extension -> FilePath -> IO (Maybe String)
moveFile dictionary path = case findTargetPath dictionary path of
    Move targetDir targetPath -> do
        doesDirectoryExist targetDir >>= \case
            False -> do
                createDirectory targetDir
                renameFile path targetPath
                pure (pure targetPath)
            True -> do
                newTargetPath <- findNewTargetPath targetPath
                renameFile path newTargetPath
                pure (pure newTargetPath)
    TargetIgnore -> pure Nothing

directoryExist :: FilePath -> IO ()
directoryExist path = do
    doesDirectoryExist path >>= \case
        True -> pure ()
        False -> do
            putStrLn $ "'" <> path <> "' does not exist or is not a directory"
            exitFailure

sortDirectory :: Map FilePath Extension -> FilePath -> IO ()
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
        (putStrLn ("Untested on operating system: " <> os) >> exitFailure)
    opts <- options
    configuration <- mapM canonicalizePath opts.config
    directory <- canonicalizePath opts.directory
    dict <- parseConfig configuration
    if opts.sortOnce then do
        sortDirectory dict directory
        putStrLn ("Sorted '" <> directory <> "'")
        exitSuccess
    else do
        directoryExist directory
        putStrLn $ "Watching " <> directory
        mgr <- startManager
        void $ watchDir mgr directory isFile (act dict)
        void $ forever (threadDelay 1_000_000)
        exitFailure
