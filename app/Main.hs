{-# LANGUAGE OverloadedStrings #-}
module Main where

import Epsilon.Internal.Interpreter
import Epsilon.Internal.Classes
import Control.Monad hiding ( fail )
import Data.Text ( Text, unpack, concat, pack )
import Data.Text.IO ( readFile, putStrLn )
import Prelude hiding ( readFile, concat, fail, putStrLn )
import Epsilon.Internal.Parser
import Control.Applicative
import Epsilon.Internal.SemanticAnalyzer
import Epsilon.CliOptions
import Data.Map.Strict hiding ( null, foldr, lookup, empty)
import System.Clock

version :: Text
version = "0.0.1.0"

autoEnv :: Map Text Function
autoEnv = fromList [
    ("+", MkFunction (Just $ InfixL 6) [] [Unnamed TInt, Unnamed TInt] TInt Nothing),
    ("-", MkFunction (Just $ InfixL 6) [] [Unnamed TInt, Unnamed TInt] TInt Nothing),
    ("*", MkFunction (Just $ InfixL 7) [] [Unnamed TInt, Unnamed TInt] TInt Nothing),
    ("/", MkFunction (Just $ InfixL 7) [] [Unnamed TInt, Unnamed TInt] TInt Nothing),
    ("^", MkFunction (Just $ InfixR 8) [] [Unnamed TInt, Unnamed TInt] TInt Nothing),
    ("==", MkFunction (Just $ Infix 4) [] [Unnamed TInt, Unnamed TInt] TBool Nothing),
    ("&&", MkFunction (Just $ InfixR 3) [] [Unnamed TBool, Unnamed TBool] TBool Nothing),
    ("||", MkFunction (Just $ InfixR 2) [] [Unnamed TBool, Unnamed TBool] TBool Nothing),
    ("print", MkFunction Nothing [Impure] [Unnamed TInt] TVoid Nothing),
    ("printStr", MkFunction Nothing [Impure] [Unnamed TString] TVoid Nothing)
    ]

fromJustIO :: Maybe a -> IO a
fromJustIO = foldr ((<|>) . pure) (fail "Main.fromJustIO: empty Maybe")

main :: IO ()
main = (do
    cf <- execParser cliFlagsO
    contents <- concat <$> mapM readFile (fmap unpack [files cf])

    let env = if automaticBuiltins cf then autoEnv else mempty

    preParse <- getTime Monotonic

    (env', ers, sts) <- runModule program (withStringAndEnv contents env)
    guard (not $ null sts) <|> putStrLn ("PARSE ERROR:\n\t" <> head ers) *> empty
    sts' <- fromJustIO sts

    preAnal <- getTime Monotonic

    (env'', ers', sts'') <- runModule (analyzeProgramm sts') (withEnvSA env')
    guard (not $ null sts'') <|> putStrLn ("SEMANTIC ANALYSIS ERROR:" <> foldr (\a acc -> acc <> "\n\t" <> a) "" ers') *> empty
    when (not $ null ers') $ putStrLn $ "SEMANTIC ANALYSIS COMPLETE:" <> foldr (\a acc -> acc <> "\n\t" <> a) "" ers' 
    sts''' <- fromJustIO sts''

    preRuntime <- getTime Monotonic

    (_, ers'', _) <- runModule (interpretStatements sts''') (withEnvI env'')
    guard (null ers'') <|> putStrLn ("RUNTIME ERROR:\n\t" <> head ers'') *> empty

    post <- getTime Monotonic

    when (timeBenchmark cf) $ do
        putStrLn "\x1b[33;1mEpsilon Timer Benchmark:\x1b[0m"

        let 
            parse = diffTimeSpec preAnal preParse
            anal = diffTimeSpec preRuntime preAnal
            runtime = diffTimeSpec post preRuntime
            toMs tm = pack (show (fromInteger @Double (toNanoSecs tm) / 1000000))
        
        putStrLn $ "\tParse Time: " <> pack (show (toNanoSecs parse)) <> " ns [ " <> toMs parse <> " ms]"
        putStrLn $ "\tAnalysis Time: " <> pack (show (toNanoSecs anal)) <> " ns [ " <> toMs anal<> " ms]"
        putStrLn $ "\tInterpreter Time: " <> pack (show (toNanoSecs runtime)) <> " ns [ " <> toMs runtime <> " ms]"


    ) <|> pure ()
