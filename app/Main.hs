{-# LANGUAGE OverloadedStrings #-}
module Main where

import Epsilon.Internal.Interpreter
import System.Environment
import Epsilon.Internal.Classes
import Control.Monad hiding ( fail )
import Data.Text ( Text, pack, uncons, unpack, concat )
import Data.Text.IO ( readFile, putStrLn )
import Prelude hiding ( readFile, concat, fail, putStrLn )
import Epsilon.Internal.Parser
import Control.Applicative
import Epsilon.Internal.SemanticAnalyzer

data EpsilonFlags =
    PrettyPrint |
    AutomaticBuiltins
    deriving (Eq, Show)

version :: Text
version = "0.0.1.0"

shorthands :: [(Text, EpsilonFlags)]
shorthands = [
    ("pp", PrettyPrint),
    ("pretty-print", PrettyPrint),
    ("autob", AutomaticBuiltins),
    ("auto-builtins", AutomaticBuiltins)
    ]

match :: [Text] -> ([Text], [EpsilonFlags])
match [] = ([],[])
match (x:xs) = case uncons x of
    Nothing -> (fs, flgs)
    Just (c, cs) -> case uncons cs of
        Just (c', cs') | c == '-' && c' == '-' -> case lookup cs' shorthands of
            Just a -> (fs, a : flgs)
            _      -> (fs, flgs)
        _          -> (x : fs, flgs)
    where
        (fs, flgs) = match xs

autoEnv :: Environment
autoEnv = [
    ("+", Function (Just $ InfixL 6) [Unnamed TInt, Unnamed TInt] TInt Nothing),
    ("-", Function (Just $ InfixL 6) [Unnamed TInt, Unnamed TInt] TInt Nothing),
    ("*", Function (Just $ InfixL 7) [Unnamed TInt, Unnamed TInt] TInt Nothing),
    ("/", Function (Just $ InfixL 7) [Unnamed TInt, Unnamed TInt] TInt Nothing),
    ("^", Function (Just $ InfixR 8) [Unnamed TInt, Unnamed TInt] TInt Nothing),
    ("==", Function (Just $ Infix 4) [Unnamed TInt, Unnamed TInt] TBool Nothing),
    ("&&", Function (Just $ InfixR 3) [Unnamed TBool, Unnamed TBool] TBool Nothing),
    ("||", Function (Just $ InfixR 2) [Unnamed TBool, Unnamed TBool] TBool Nothing),
    ("print", Function Nothing [Unnamed TInt] TVoid Nothing),
    ("printStr", Function Nothing [Unnamed TString] TVoid Nothing)
    ]

fromJustIO :: Maybe a -> IO a
fromJustIO = foldr ((<|>) . pure) (fail "Main.fromJustIO: empty Maybe")

main :: IO ()
main = (do
    args <- getArgs
    let (files, flags) = match (fmap pack args)
    let env = if elem AutomaticBuiltins flags then autoEnv else []
    contents <- concat <$> mapM readFile (fmap unpack files)

    (env', ers, sts) <- runModule program (withStringAndEnv contents env)
    guard (not $ null sts) <|> putStrLn ("PARSE ERROR:\n\t" <> head ers) *> empty
    sts' <- fromJustIO sts

    (env'', ers', sts'') <- runModule (analyzeProgramm sts') (withEnvSA env')
    guard (not $ null sts'') <|> putStrLn ("SEMANTIC ANALYSIS ERROR:" <> foldr (\a acc -> acc <> "\n\t" <> a) "" ers') *> empty
    when (not $ null ers') $ putStrLn $ "SEMANTIC ANALYSIS COMPLETE:" <> foldr (\a acc -> acc <> "\n\t" <> a) "" ers' 
    sts''' <- fromJustIO sts''

    (_, ers'', _) <- runModule (interpretStatements sts''') (withEnvI env'')
    guard (not $ null ers'') <|> putStrLn ("RUNTIME ERROR:\n\t" <> head ers'') *> empty
    ) <|> pure ()
