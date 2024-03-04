module Parser (parseConfig) where

import Control.Monad (unless)
import Data.Char (isAlpha)
import Data.Map (Map)
import Data.Map qualified as Map
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import Text.ParserCombinators.ReadP (
    ReadP,
    char,
    eof,
    many1,
    readP_to_S,
    satisfy,
    sepBy,
    skipSpaces,
    string,
 )

parseConfig :: FilePath -> IO (Map FilePath FilePath)
parseConfig file = do
    b <- doesFileExist file
    unless b (putStrLn $ "'" <> file <> "' does not exist")
    input <- readFile file
    parseText input

parseText :: String -> IO (Map FilePath FilePath)
parseText input = case readP_to_S fileP input of
    [(xs, "")] -> return (Map.fromList xs)
    _else -> putStrLn "Failed parsing config file" >> exitFailure

fileP :: ReadP [(String, String)]
fileP = sepBy lineP (char '\n') <* skipSpaces <* eof

lineP :: ReadP (String, String)
lineP = do
    skipSpaces
    left <- anyString
    skipSpaces
    _ <- string "->"
    skipSpaces
    right <- anyString
    pure (left, right)

anyString :: ReadP String
anyString = many1 (satisfy isAlpha)
