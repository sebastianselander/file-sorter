module Parser (parseConfig, Extension(..)) where

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

data Extension = ExtIgnore | Ext !String
    deriving (Show, Eq, Ord)

parseConfig :: Maybe FilePath -> IO (Map FilePath Extension)
parseConfig Nothing = pure mempty
parseConfig (Just file) = do
    b <- doesFileExist file
    unless b (putStrLn $ "'" <> file <> "' does not exist")
    input <- readFile file
    parseText input

parseText :: String -> IO (Map FilePath Extension)
parseText input = case readP_to_S fileP input of
    [(xs, "")] -> return (Map.fromList xs)
    _else -> putStrLn "Failed parsing config file" >> exitFailure

fileP :: ReadP [(String, Extension)]
fileP = sepBy lineP (char '\n') <* skipSpaces <* eof

lineP :: ReadP (String, Extension)
lineP = do
    skipSpaces
    left <- anyString
    skipSpaces
    _ <- string "->"
    skipSpaces
    right <- anyString
    case right of
        "ignore" -> pure (left, ExtIgnore)
        _        -> pure (left, Ext right)

anyString :: ReadP String
anyString = many1 (satisfy isAlpha)
