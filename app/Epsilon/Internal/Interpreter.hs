{-# LANGUAGE LambdaCase, TemplateHaskell, RecordWildCards, OverloadedStrings, RebindableSyntax #-}

module Epsilon.Internal.Interpreter where

import Control.Monad hiding ( MonadFail(..) )
import Control.Applicative
import Control.Monad.IO.Class
import Data.String
import Data.Text hiding ( empty, zip, foldl, foldl1, foldr, filter )
import Data.Text.IO
import Epsilon.Internal.Classes
import Optics
import Prelude hiding ( MonadFail(..), putStrLn )

newtype Interpreter m a = Interpreter { runInterpreter :: InterpreterState -> m (Either InterpreterState (InterpreterState, a)) }

data InterpreterState = IS {
    _environment :: Environment,
    _backlog :: [Text],
    _stacktrace :: [Text],
    _crashed :: Bool
}

instance Semigroup InterpreterState where

instance Monoid InterpreterState where
    mempty = IS [] [] [] False

makeLenses ''InterpreterState

getEnv :: Interpreter IO Environment
getEnv = (^. environment) <$> get

putEnv :: Environment -> Interpreter IO ()
putEnv e = modify (environment .~ e)

modifyEnv :: (Environment -> Environment) -> Interpreter IO ()
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
    fail s = Interpreter $ \st -> pure $ Left ((crashed .~ True) . (backlog %~ (format s st :)) $ st)
        where
            format s st = s <> "\nSTACK TRACE:\n\t" <> intercalate "\n\t" (st ^. stacktrace ++ ["<top level>"])

instance EpsilonModule (Interpreter IO) InterpreterState IO where
    runModule st s = do
        l <- runInterpreter st s
        pure $ case l of
            Left l -> (l ^. environment, l ^. backlog, Nothing)
            Right (r,a) -> (r ^. environment, [], Just a)
    ctor f = Interpreter $ \st -> Right <$> f st
    error = fail
    warn = fail
    get = Interpreter $ \s -> pure $ Right $ (s,s)
    put s = Interpreter $ const $ pure $ Right (s,())



handleBuiltin :: (MonadIO m, MonadFail m) => Text {- name -} -> [Value] -> Interpreter m Value
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
    _ -> fail $ "No pattern for builtin function '" <> s <> "'"

evalExp :: Expression -> Interpreter IO Value
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
            _              -> fail $ "No entry with name '" <> s <> "'"
    ApplyFun x@(Lookup _) params -> do
        func <- evalExp x
        vals <- mapM evalExp params
        case func of
            VFunction s (Function _ ps r (Just instrs)) -> runFunction (zip vals ps) instrs s r
            VFunction s (Function _ _ _ Nothing) -> handleBuiltin s vals
            _ -> fail "Expression does not reference a function"
    ApplyFun _ _ -> fail "Function invocation for non-lookup expressions is not supported"

runFunction :: [(Value, Param)] -> [Statement] -> Text {- name -} -> EpsilonType {- returnType -} -> Interpreter IO Value
runFunction vps func n rt = do
    e <- getEnv
    forM_ vps (\(val, par) -> case par of
        Unnamed _ -> fail "Unnamed params not allowed in non-builtin function or operator"
        WellTyped t n -> if typeOf val == t then modifyEnv ((n, Value val t) :) else fail $ "Couldn't match type " <> showE t <> " of parameter '" <> n <> "' with actual type " <> showE (typeOf val) <> "\n\tin the function invocation of " <> n
        Inferred n -> modifyEnv ((n, Value val (typeOf val)) :)
        )
    modify $ over stacktrace ((:) $ n <> "(" <> intercalate " , " (fmap showType vps) <> ")")
    interpretStatements func
    e' <- getEnv
    putEnv $ e --drop (length e' - length e) e'
    case lookup "$RETURN" e' of
        Just (Value v t) -> if t == rt then pure v else fail $ "Couldn't match type " <> showE t <> " with return type " <> showE rt <> "\n\tin the function invocation of " <> n
        Just _ -> fail "Illegal return value"
        _ | rt == TVoid -> pure VVoid
        _               -> fail $ "Missing non-void return value of type " <> showE rt <> "\n\tin the function invocation of " <> n
    where
            showType (_,Unnamed t) = toLower $ showE t
            showType (_,WellTyped t _) = toLower $ showE t
            showType (_,Inferred _) = "<inferred>" 
    
     

interpretStatements :: [Statement] -> Interpreter IO ()
interpretStatements = mapM_ evalStatement

typeOf :: Value -> EpsilonType
typeOf (VInt _) = TInt
typeOf (VBool _) = TBool
typeOf (VString _) = TString
typeOf (VFloat _) = TFloat
typeOf (VVoid) = TVoid
typeOf (VFunction _ _) = TFunction


updateEnvironment :: MonadFail m => Text -> Value -> Environment -> Interpreter m Environment
updateEnvironment s v [] = pure [(s, Value v (typeOf v))]
updateEnvironment s v (x@(s',v'):xs)
    | s /= s' = fmap (x:) $ updateEnvironment s v xs
    | Value v'' t <- v' = case t == typeOf v of
        True -> pure $ (s, Value v t) : xs
        _    -> fail $ "Couldn't match type " <> (pack $ show t) <> " (ILT: " <> (pack $ show  v'') <> ") of field '" <> s <> "' with actual type " <> (pack $ show $ typeOf v)
    | otherwise = fmap (x:) $ updateEnvironment s v xs

evalStatement :: Statement -> Interpreter IO ()
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
                    VarSet _ _ -> fail "Cannot override literal value"          


withEnvI :: Environment -> InterpreterState
withEnvI env = (environment .~ env) mempty