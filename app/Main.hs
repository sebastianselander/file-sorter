{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Concurrent
import Data.List (isInfixOf)
import Magic
import System.FSNotify
import System.FilePath
import Language.Haskell.TH

move :: String -> Q Dec
move nm = do
    name <- newName "move"
    funD name [clau]
  where
    clau :: Q Clause
    clau = do 
        Just f <- lookupValueName "isInfixOf"
        let firstE = appE (varE f) (litE (StringL "pdf"))
        let secondE = litE (StringL "blopdfblo")
        expr <- appE firstE secondE
        guardPat <- normalG (return expr)
        let body = guardedB [return (guardPat, LitE (CharL 'a'))]
        name <- newName "x"
        let ma = match (varP name) body []
        undefined
    

mkPath :: FilePath -> FilePath
mkPath p = concat ["/", p, "/"]

pdf, pdfPath :: FilePath
pdf = "pdf"
pdfPath = mkPath pdf

jpeg, jpegPath :: FilePath
jpeg = "jpeg"
jpegPath = mkPath jpeg

png, pngPath :: FilePath
png = "jpeg"
pngPath = mkPath png

sleepSecs :: Int -> IO ()
sleepSecs n = threadDelay (n * 1_000_000)

shouldAct :: Event -> Bool
shouldAct (Added{}) = True
shouldAct _ = False

mkNewName :: FilePath -> FilePath -> FilePath
mkNewName oldpath dir = takeDirectory oldpath <> dir <> takeFileName oldpath

moveFile :: FilePath -> String -> IO ()
moveFile oldname mime
    | pdf `isInfixOf` mime = putStrLn $ "New path: " <> mkNewName oldname pdfPath
    | jpeg `isInfixOf` mime = putStrLn $ "New path: " <> mkNewName oldname jpegPath
    | png `isInfixOf` mime = putStrLn $ "New path: " <> mkNewName oldname pngPath
    | otherwise = return ()

act :: Event -> IO ()
act (Added path _ IsFile) = do
    magic <- magicOpen [MagicMime]
    magicLoadDefault magic
    mime <- magicFile magic path
    putStrLn $ "Mime: " <> mime
    putStrLn $ "Path: " <> path
    putStrLn ""
    moveFile path mime
act _ = return ()

main :: IO ()
main = do
    putStrLn "Running..."
    mgr <- startManager
    _ <- watchDir mgr "." shouldAct act
    _ <- getLine
    stopManager mgr
    putStrLn "Stopping..."

-- hello
