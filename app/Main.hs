module Main where

import Epsilon.Internal.Interpreter
import System.Environment
import Epsilon.Internal.Parser
import Control.Monad


data EpsilonFlags =
    PrettyPrint |
    AutomaticBuiltins
    deriving (Eq, Show)

shorthands :: [(String, EpsilonFlags)]
shorthands = [
    ("pp", PrettyPrint),
    ("pretty-print", PrettyPrint),
    ("autob", AutomaticBuiltins),
    ("auto-builtins", AutomaticBuiltins)
    ]

match :: [String] -> ([String], [EpsilonFlags])
match [] = ([],[])
match (('-':'-':x):xs) = case lookup x shorthands of
    Just ef -> (ss, ef : efs)
    _       -> (ss, efs)
    where
        (ss, efs) = match xs
match (x:xs) = (x : ss, efs)
    where
        (ss, efs) = match xs

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

main :: IO ()
main = do
    args <- getArgs
    let (files, flags) = match args
    let env = if elem AutomaticBuiltins flags then autoEnv else []
    contents <- concat <$> mapM readFile files
    parseThenInterpret contents env
    
