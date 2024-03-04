module Options (Options (..), options) where

import Options.Applicative

data Options = Options
    { sortOnce :: !Bool
    , config :: !(Maybe FilePath)
    , directory :: !FilePath
    } deriving (Show)

options :: IO Options
options = execParser (info (Options <$> sortParser <*> configParser <*> directoryParser) fullDesc)

sortParser :: Parser Bool
sortParser =
    flag
        False
        True
        ( long "sort-once"
            <> short 's'
            <> help desc
        )
  where
    desc =
        "Should the directory and all its contents be sorted once immediately"

configParser :: Parser (Maybe FilePath)
configParser =
    optional $ strOption
        ( long "config"
            <> short 'c'
            <> metavar "<FILE>"
            <> help "Optional extension to directory mapping configuration file"
        )

directoryParser :: Parser FilePath
directoryParser = argument str (metavar "<FILE>")
