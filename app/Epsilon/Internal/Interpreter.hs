{-# LANGUAGE LambdaCase, TemplateHaskell, RecordWildCards #-}

module Epsilon.Internal.Interpreter where

import Epsilon.Internal.Parser hiding ( modify, _environment, environment, _backlog, backlog, _crashed, crashed )
import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad
import Optics
import qualified Epsilon.Internal.Parser as EIP
import Debug.Trace

newtype Interpreter m a = Interpreter { runInterpreter :: InterpreterState -> m (Either InterpreterState (InterpreterState, a))}

data InterpreterState = IS {
    _environment :: Environment,
    _backlog :: [String],
    _crashed :: Bool
} deriving Show

instance Semigroup InterpreterState where

instance Monoid InterpreterState where
    mempty = IS [] [] False

makeLenses ''InterpreterState

get :: Applicative m => Interpreter m InterpreterState
get = Interpreter $ \s -> pure $ pure (s,s)

getEnv :: Applicative m => Interpreter m Environment
getEnv = (^. environment) <$> get

put :: Applicative m => InterpreterState -> Interpreter m ()
put s = Interpreter $ const $ pure $ pure (s,())

putEnv :: Monad m => Environment -> Interpreter m ()
putEnv e = modify (environment .~ e)

modify :: Monad m => (InterpreterState -> InterpreterState) -> Interpreter m ()
modify f = get >>= put . f

modifyEnv :: Monad m => (Environment -> Environment) -> Interpreter m ()
modifyEnv f = modify (over environment f)

instance Functor m => Functor (Interpreter m) where
    fmap f (Interpreter st) = Interpreter $ \s -> (fmap $ \(a,b) -> (a, f b)) <$> st s

instance Monad m => Applicative (Interpreter m) where
    pure x = Interpreter $ \s -> pure $ pure (s,x)
    (Interpreter stf) <*> (Interpreter sta) = Interpreter $ \s -> do
        e <- stf s
        case e of
            Right (s', f) -> do
                e' <- sta s'
                case e' of
                    Right (s'', a) -> pure $ Right (s'', f a)
                    Left l'        -> pure $ Left l'
            Left l -> pure $ Left l


instance Monad m => Monad (Interpreter m) where
    (Interpreter st) >>= f = Interpreter $ \s -> st s >>= \case 
        Right (s', a) -> runInterpreter (f a) s'
        Left l        -> pure $ Left l

instance Monad m => Alternative (Interpreter m) where
    empty = fail "<Undisclosed Runtime Error>"
    (Interpreter sta) <|> (Interpreter stb) = Interpreter $ \s -> sta s >>= (\case
        Left _ -> stb s
        Right r -> pure $ Right r
        )

instance MonadPlus m => MonadPlus (Interpreter m) where
    mzero = empty
    mplus = (<|>)

instance MonadIO m => MonadIO (Interpreter m) where
    liftIO io = Interpreter $ \s -> Right . (s,) <$> liftIO io

instance Monad m => MonadFail (Interpreter m) where
    fail s = Interpreter $ \st -> pure $ Left ((crashed .~ True) . (backlog %~ (s :)) $ st)

handleBuiltin :: (MonadIO m, MonadFail m) => String {- name -} -> [Value] -> Interpreter m Value
handleBuiltin s vs = case (s, vs) of
    ("+", [VInt a, VInt b]) -> pure $ VInt $ a + b
    ("-", [VInt a, VInt b]) -> pure $ VInt $ a - b
    ("*", [VInt a, VInt b]) -> pure $ VInt $ a * b
    ("/", [VInt a, VInt b]) -> pure $ VInt $ div a b
    ("^", [VInt a, VInt b]) -> pure $ VInt $ a ^ b
    ("==", [VInt a, VInt b]) -> pure $ VBool $ a == b
    ("<", [VInt a, VInt b]) -> pure $ VBool $ a < b
    (">", [VInt a, VInt b]) -> pure $ VBool $ a > b
    ("||", [VBool a, VBool b]) -> pure $ VBool $ a || b
    ("&&", [VBool a, VBool b]) -> pure $ VBool $ a && b
    ("print", [VInt a]) -> VVoid <$ liftIO (print a)
    ("printStr", [VString s]) -> VVoid <$ liftIO (putStrLn s)
    _ -> fail $ "No pattern for builtin function '" ++ s ++ "'"

evalExp :: (MonadIO m, MonadFail m) => Expression -> Interpreter m Value
evalExp = \case
    IntLit i -> pure $ VInt i
    StringLit s -> pure $ VString s
    FloatLit f -> pure $ VFloat f
    BoolLit b -> pure $ VBool b
    Lookup s -> do
        st <- get
        --trace (show $ map fst $ _environment st) $ pure ()
        case lookup s (st ^. environment) of
            Just (Value l _) -> pure l
            Just fe        -> pure $ VFunction s fe
            _              -> fail $ "No entry with name '" ++ s ++ "'"
    ApplyFun x params -> do
        func <- evalExp x
        vals <- mapM evalExp params
        case func of
            VFunction s (Function _ ps _ (Just instrs)) -> runFunction (zip vals ps) instrs
            VFunction s (Function _ _ _ Nothing) -> handleBuiltin s vals
            _ -> fail "Expression does not reference a function"

runFunction :: (MonadIO m, MonadFail m) => [(Value, Param)] -> [Statement] -> Interpreter m Value
runFunction vps func = do
    e <- getEnv
    forM_ vps (\(val, par) -> case par of
        Unnamed _ -> fail "Unnamed params not allowed in non-builtin function or operator"
        WellTyped t n -> if typeOf val == t then modifyEnv ((n, Value val t) :) else fail $ "Couldn't match type " ++ show t ++ " of parameter '" ++ n ++ "' with actual type " ++ show (typeOf val)
        Inferred n -> modifyEnv ((n, Value val (typeOf val)) :)
        )
    interpretStatements func
    e' <- getEnv
    putEnv $ e --drop (length e' - length e) e'
    case lookup "$RETURN" e' of
        Just (Value v _) -> pure v
        Just _ -> fail "Illegal return value"
        _ -> pure VVoid
    
     

interpretStatements :: (MonadIO m, MonadFail m) => [Statement] -> Interpreter m ()
interpretStatements = mapM_ evalStatement

typeOf :: Value -> EpsilonType
typeOf (VInt _) = TInt
typeOf (VBool _) = TBool
typeOf (VString _) = TString
typeOf (VFloat _) = TFloat
typeOf (VVoid) = TVoid


updateEnvironment :: MonadFail m => String -> Value -> Environment -> Interpreter m Environment
updateEnvironment s v [] = pure [(s, Value v (typeOf v))]
updateEnvironment s v (x@(s',v'):xs)
    | s /= s' = fmap (x:) $ updateEnvironment s v xs
    | Value v'' t <- v' = case t == typeOf v of
        True -> pure $ (s, Value v t) : xs
        _    -> fail $ "Couldn't match type " ++ show t ++ " (ILT: " ++ show v'' ++ ") of field '" ++ s ++ "' with actual type " ++ show (typeOf v)
    | otherwise = fmap (x:) $ updateEnvironment s v xs

evalStatement :: (MonadIO m, MonadFail m) => Statement -> Interpreter m ()
evalStatement s = do
    envir <- getEnv
    case lookup "$RETURN" envir of
        Just _ -> pure ()
        _      -> case s of
                    VarSet (Lookup l) res -> do
                        exp <- evalExp res
                        e <- getEnv
                        updateEnvironment l exp e >>= putEnv
                    If exp st -> do
                        VBool b <- evalExp exp
                        if b then interpretStatements st else pure ()
                    IfElse exp st1 st2 -> do
                        VBool b <- evalExp exp
                        if b then interpretStatements st1 else interpretStatements st2
                    While exp st1 -> do
                        VBool b <- evalExp exp
                        if b then interpretStatements st1 *> evalStatement (While exp st1) else pure ()
                    Action exp -> do
                        void $ evalExp exp
                    Return exp -> do
                        x <- evalExp exp
                        e <- getEnv
                        updateEnvironment "$RETURN" x e >>= putEnv
                    EnvironmentChanged -> pure ()           


parseThenInterpret :: (MonadFail m, MonadIO m) => String -> m ()
parseThenInterpret s = case runParser program (EIP.string .~ s $ mempty) of
    Left (PS { _backlog = bl, .. }) -> liftIO $ putStrLn $ foldl (\acc a -> acc ++ "\n\t" ++ a) "PARSE ERROR: " bl
    Right (PS { _environment = env, ..}, sts) -> runInterpreter (interpretStatements sts) (environment .~ env $ mempty) >>= (\case
        Left (IS { _backlog = bl }) -> liftIO $ putStrLn $ foldl (\acc a -> acc ++ "\n\t" ++ a) "RUNTIME ERROR: " bl
        Right _                     -> pure ()
        )