{-|
Module : Epsilon.CliOptions
Description : Commandline arguments parser using optparse-applicative
Copyright : (c) Thrithralas 2022
License : None
Maintainer : marci.petes@gmail.com
Stability : experimental
Description : 
The definion of CLI argument flags and their parsers. Note that the parsers here aren't affiliated with the parsers in `Epsilon.Internal.Parser`.
-}

module Epsilon.CliOptions ({- | Execute the optparse-applicative parser into an `IO` context. -}execParser, CliFlags(..), cliFlagsO) where

import Options.Applicative
import Data.Text

-- | Flags used by the compiler.
data CliFlags = CF {
    files :: Text, -- ^ The file to parse.
    automaticBuiltins :: Bool, -- ^ Wether to import automatic builtins or not.
    inlineDepth :: Int, -- ^ The depth at which functions are inlined.
    debugMessages :: Bool, -- ^ Wether to show debug messages or not.
    timeBenchmark :: Bool -- ^ Wether to time each step.
}

-- | The optparse-applicative parser for the `CliFlags`. For everyday usage, please see `cliFlagsO`.
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
    <*> switch (
        long "time"
        <> short 't'
        <> help "Time each compilation module"
    )

-- | Constructs a `ParserInfo` from the parser, to assign a description and a helper.
cliFlagsO :: ParserInfo CliFlags
cliFlagsO = info (cliFlags <**> helper) (
    fullDesc
    <> progDesc "Compile an Epsilon program"
    )