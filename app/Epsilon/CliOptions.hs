module Epsilon.CliOptions (execParser, CliFlags(..), cliFlagsO) where

import Options.Applicative
import Data.Text

data CliFlags = CF {
    files :: Text,
    automaticBuiltins :: Bool,
    inlineDepth :: Int,
    debugMessages :: Bool
}

cliFlags :: Parser CliFlags
cliFlags = CF
    <$> strOption (
        long "input"
        <> short 'i'
        <> metavar "FILE"
        <> help "File to parse, analyze and interpret"
    )
    <*> switch (
        long "auto-builtins"
        <> short 'b'
        <> help "Automatically import parts of the standard library"
    )
    <*> option auto (
        long "inline-depth"
        <> short 'I'
        <> help "Define inline depth"
        <> value 1
        <> metavar "DEPTH"
    )
    <*> switch (
        long "debug"
        <> short 'd'
        <> help "Print helpful debug messages and monad states"
    )

cliFlagsO :: ParserInfo CliFlags
cliFlagsO = info (cliFlags <**> helper) (
    fullDesc
    <> progDesc "Compile an Epsilon program"
    )