{-# LANGUAGE LambdaCase #-}
module Epsilon.Interpreter where

import Epsilon.Internal.Constructs
import Control.Monad.IO.Class
import Data.Functor
import Epsilon.Parser
import Optics
import Control.Monad

handleBuiltin :: IOContext m => String {- name -} -> [Value] -> State m Environment Value
handleBuiltin s vs = case (s, vs) of
    ("+", [VInt a, VInt b]) -> pure $ VInt $ a + b
    ("-", [VInt a, VInt b]) -> pure $ VInt $ a - b
    ("*", [VInt a, VInt b]) -> pure $ VInt $ a * b
    ("/", [VInt a, VInt b]) -> pure $ VInt $ div a b
    ("^", [VInt a, VInt b]) -> pure $ VInt $ a ^ b
    ("==", [VInt a, VInt b]) -> pure $ VBool $ a == b
    ("<", [VInt a, VInt b]) -> pure $ VBool $ a < b
    (">", [VInt a, VInt b]) -> pure $ VBool $ a > b
    ("print", [VInt a]) -> VVoid <$ liftIO (print a)
    ("printStr", [VString s]) -> VVoid <$ liftIO (putStrLn s)
    _ -> fail $ "No pattern for builtin function '" ++ s ++ "'"

evalExp :: IOContext m => Expression -> State m Environment Value
evalExp = \case
    IntLit i -> pure $ VInt i
    StringLit s -> pure $ VString s
    FloatLit f -> pure $ VFloat f
    BoolLit b -> pure $ VBool b
    Lookup s -> do
        env <- get
        case lookup s env of
            Just (Field l) -> pure l
            Just fe        -> pure $ VFunction s fe
            _              -> fail $ "No entry with name '" ++ s ++"'!"
    ApplyFun x params -> do
        func <- evalExp x
        vals <- mapM evalExp params
        case func of
            VFunction s (Function ps _ instrs) -> runFunction (zip vals ps) instrs
            VFunction s (Operator _ ps _ instrs) -> runFunction (zip vals ps) instrs
            VFunction s (BuiltinFunction _ _) -> handleBuiltin s vals
            VFunction s (BuiltInOperator _ _ _) -> handleBuiltin s vals
            _ -> fail "Expression does not reference a function"

runFunction :: IOContext m => [(Value, Param)] -> [Statement] -> State m Environment Value
runFunction vps func = do
    e <- get
    forM_ vps (\(val, par) -> case par of
        Unnamed _ -> fail "Unnamed params not allowed in non-builtin function or operator"
        Typed n _ -> modify ((n, Field val) :)
        Inferred n -> modify ((n, Field val) :)
        )
    runInterpreter func
    e' <- get
    put e
    case lookup "$RETURN" e' of
        Just (Field v) -> pure v
        Just _ -> fail "Illegal return value"
        _ -> pure VVoid
    
     

runInterpreter :: IOContext m => [Statement] -> State m Environment ()
runInterpreter = mapM_ evalStatement

updateEnvironment :: String -> Value -> Environment -> Environment
updateEnvironment s v [] = [(s, Field v)]
updateEnvironment s v (x@(s',v'):xs)
    | s == s' = (s, Field v) : xs
    | otherwise = x : updateEnvironment s v xs

evalStatement :: IOContext m => Statement -> State m Environment ()
evalStatement s = do
    envir <- get
    case lookup "$RETURN" envir of
        Just _ -> pure ()
        _      -> case s of
                    VarSet (Lookup l) res -> do
                        exp <- evalExp res
                        void $ modify $ updateEnvironment l exp
                    If exp st -> do
                        VBool b <- evalExp exp
                        if b then runInterpreter st else pure ()
                    IfElse exp st1 st2 -> do
                        VBool b <- evalExp exp
                        if b then runInterpreter st1 else runInterpreter st2
                    While exp st1 -> do
                        VBool b <- evalExp exp
                        if b then runInterpreter st1 *> evalStatement (While exp st1) else pure ()
                    Action exp -> do
                        void $ evalExp exp
                    Return exp -> do
                        x <- evalExp exp
                        void $ modify $ updateEnvironment "$RETURN" x
                    EnvironmentChanged -> pure ()           

interpret :: IOContext m => String -> State m Environment ()
interpret t = case runState program (string .~ t $ defaultParserState) of
    Nothing -> fail "Parse error"
    Just (p, sts) -> put (p ^. env) *> runInterpreter sts
