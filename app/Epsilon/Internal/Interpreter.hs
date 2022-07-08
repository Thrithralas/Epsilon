{-# LANGUAGE LambdaCase, TemplateHaskell, RecordWildCards, OverloadedStrings, RebindableSyntax #-}
{-|
Module : Epsilon.Internal.Interpreter
Description : The runtime of the programming language.
Copyright : (c) Thrithralas 2022
License : None
Maintainer : marci.petes@gmail.com
Stability : experimental
Description : 
The native-haskell interpreter executing the application using state monads. The interpreter is fully prepared to handle errors the semantic analyzer usually handles (due to it preceding the analyzer), but this is more of a safety net for unimplemented features rather than something to be abused.

The interpreter supports memoization for pure functions and will do so without any user intervention. To prevent a function from being memoized, refer to the IMPURE pragma.
-}
module Epsilon.Internal.Interpreter where

import Control.Monad hiding ( MonadFail(..) )
import Control.Applicative
import Control.Monad.IO.Class
import Data.String
import Data.Text hiding ( empty, zip, foldl, foldl1, foldr, filter, singleton )
import Data.Text.IO
import Epsilon.Internal.Classes
import Optics
import Prelude hiding ( MonadFail(..), putStrLn, lookup )
import Data.Map.Strict as Map

-- | The state monad representing the interpreter. While it technically accepts any (* -> *) type as a wrapper, it will only work with IO due to certain conditions.
newtype Interpreter m a = Interpreter { runInterpreter :: InterpreterState -> m (Either InterpreterState (InterpreterState, a)) }

-- | The internal state of the interpreter.
data InterpreterState = IS {
    _environment :: Environment, -- ^ The environment.
    _backlog :: [Text], -- ^ A backlog of errors and warnings to report.
    _stacktrace :: [Text], -- ^ The stack trace, printed alongside errors.
    _crashed :: Bool, -- ^ Wether the interpreter failed or not
    _memoizationTable :: Map Text (Map [Value] Value) -- ^ The map containing all memoized records for all functions.
}

-- | WARNING! DO NOT USE `(<>)` OVER THE INTERPRETER STATE AS IT IS NOT IMPLEMENTED.
instance Semigroup InterpreterState where

instance Monoid InterpreterState where
    mempty = IS mempty [] [] False (fromList [])

-- * TH Generated Lenses
makeLenses ''InterpreterState
-- * Auxilliary Functions

-- | Applies an extra optic operation to only get the environment, rather than the entire state.
getEnv :: Interpreter IO Environment
getEnv = (^. environment) <$> get

-- | Applies an extra optic operation to update the state's environment. This uses `modify` rather than `put`.
putEnv :: Environment -> Interpreter IO ()
putEnv e = modify (environment .~ e)

-- | Applies an extra optic operation to modify the state's environment.
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
    mzero = Control.Applicative.empty
    mplus = (<|>)

instance MonadIO m => MonadIO (Interpreter m) where
    liftIO io = Interpreter $ \s -> Right . (s,) <$> liftIO io

instance Monad m => MonadFail (Interpreter m) where
    fail s = Interpreter $ \st -> pure $ Left ((crashed .~ True) . (backlog %~ (format s st :)) $ st)
        where
            format s st = s <> "\nSTACK TRACE:\n\t" <> intercalate "\n\t" (st ^. stacktrace ++ ["<top level>"])

instance EpsilonModule (Interpreter IO) InterpreterState where
    runModule st s = do
        l <- runInterpreter st s
        pure $ case l of
            Left l -> (l ^. environment, l ^. backlog, Nothing)
            Right (r,a) -> (r ^. environment, [], Just a)
    error = fail
    warn = fail
    get = Interpreter $ \s -> pure $ Right $ (s,s)
    put s = Interpreter $ const $ pure $ Right (s,())


-- | The ugly hardcoded function to handle builtin functions. This is the ugly eyesore which forces the interpreter to use the `IO` monad.
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

-- | Evaluate an expression and put its value into the interpreter.
evalExp :: Expression -> Interpreter IO Value
evalExp = \case
    IntLit i -> pure $ VInt i
    StringLit s -> pure $ VString s
    FloatLit f -> pure $ VFloat f
    BoolLit b -> pure $ VBool b
    Lookup s -> do
        st <- get
        --trace (show $ map fst $ _environment st) $ pure ()
        case lookup s (st ^. (environment % valTable)) of
            Just (l,_) -> pure l
            _              -> fail $ "No entry with name '" <> s <> "'"
    ApplyFun (Lookup s) params -> do
        env <- getEnv <&> (^. functionTable)
        vals <- mapM evalExp params
        case env !? s of
            Just (MkFunction _ fs ps r (Just instrs)) -> runFunction (zip vals ps) instrs s r fs
            Just (MkFunction _ _ _ _ Nothing) -> handleBuiltin s vals
            _ -> fail $  "Expression does not reference a function " <> (pack $ show s)
    ApplyFun _ _ -> fail "Function invocation for non-lookup expressions is not supported"

{- | Breaks down a function into its necessary arguments and interprets a function invocation. Functions run in their isolated environment, so they cannot alter anything outside of their scope, meaning beside `IO` they are all technically pure. -}
runFunction :: [(Value, Param)] {- | The values matched to the parameters. -} -> [Statement] {- | The body of the function. -} -> Text {- | The name of the function. -} -> EpsilonType {- The return type of the function. -} -> [FunctionFlags] {- Any flags applied to the function. -} -> Interpreter IO Value {- |  The result value wrapped in the interpreter. -}
runFunction vps func n rt fl = do
    mt <- get <&> (^. memoizationTable)
    case (notElem Impure fl, mt !? n) of
        (True, Just mp) | Just v <- mp !? (fmap fst vps) -> pure v
        _ -> do
            e <- getEnv
            forM_ vps (\(val, par) -> case par of
                Unnamed _ -> fail "Unnamed params not allowed in non-builtin function or operator"
                WellTyped t n | t == typeOf val -> modifyEnv (over valTable $ insert n (val, t))
                WellTyped t n -> fail $ "Couldn't match type " <> showE t <> " of parameter '" <> n <> "' with actual type " <> showE (typeOf val) <> "\n\tin the function invocation of " <> n
                Inferred n -> modifyEnv (over valTable $ insert n (val, typeOf val))
                )
            modify $ over stacktrace ((:) $ n <> "(" <> intercalate " , " (fmap showType vps) <> ")")
            interpretStatements func
            e' <- getEnv <&> (^. valTable)
            putEnv $ e --drop (length e' - length e) e'
            case lookup "$RETURN" e' of
                Just (v,t) -> 
                    if t /= rt 
                    then fail $ "Couldn't match type " <> showE t <> " with return type " <> showE rt <> "\n\tin the function invocation of " <> n
                    else v <$ modify (over memoizationTable $ insertWithKey (\k t a -> case k == n of
                            True -> union t a
                            False -> a
                            ) n (singleton (fmap fst vps) v))
                _ | rt == TVoid -> pure VVoid
                _               -> fail $ "Missing non-void return value of type " <> showE rt <> "\n\tin the function invocation of " <> n
    where
            showType (_,Unnamed t) = toLower $ showE t
            showType (_,WellTyped t _) = toLower $ showE t
            showType (_,Inferred _) = "<inferred>" 
    
     
-- | The root of the interpreter, executing the given list of statements.
interpretStatements :: [Statement] -> Interpreter IO ()
interpretStatements = mapM_ evalStatement

-- | Converts a value to its type. Not to be confused with `Epsilon.Internal.SemanticAnalyzer.typeOf` from the semantic analyzer, which evaluates an expression, but it can fail while this one can't.
typeOf :: Value -> EpsilonType
typeOf (VInt _) = TInt
typeOf (VBool _) = TBool
typeOf (VString _) = TString
typeOf (VFloat _) = TFloat
typeOf (VVoid) = TVoid

-- | Updates the environment according to a key. Common usage for this function is as follows
--
-- @
-- exp <- evalExp myExp
-- env <- getEnv \<&\> (^. valTable)
-- updateEnvironment wh exp env >>= modifyEnv . (valTable .~)
-- @
--
-- Or using `Applicative` syntax:
--
-- @
-- (\\x e -> updateEnvironment x e >>= modifyEnv . (valTable .~)) \<$\> evalExp myExp \<*\> (getEnv \<&\> (^. valTable))
-- @
updateEnvironment :: MonadFail m => Text {- ^ Name of the variable -} -> Value {- ^ The value to update to -} -> (Map Text (Value, EpsilonType)) {- ^ The map containing the interpreter data for types and values. This is always taken directly from the environment. -} -> Interpreter m (Map Text (Value, EpsilonType)) {- ^ The new lookup table. This should immidiatelly be put into the state afterwards. -}
updateEnvironment s v map | Just (_,t) <- map !? s = 
    if t == typeOf v 
    then pure (insert s (v,t) map) 
    else fail $ "Couldn't match type " <> (pack $ show t) <> " of field '" <> s <> "' with actual type " <> (pack $ show $ typeOf v) 
updateEnvironment s v map = pure $ insert s (v, typeOf v) map

-- | Evaluates a single statement.
evalStatement :: Statement -> Interpreter IO ()
evalStatement s = do
    envir <- getEnv <&> (^. valTable)
    case lookup "$RETURN" envir of
        Just _ -> pure ()
        _      -> case s of
                    VarSet (Lookup l) res -> do
                        exp <- evalExp res
                        e <- getEnv <&> (^. valTable)
                        updateEnvironment l exp e >>= modifyEnv . (valTable .~)
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
                        e <- getEnv <&> (^. valTable)
                        updateEnvironment "$RETURN" x e >>= modifyEnv . (valTable .~)
                    EnvironmentChanged _ -> pure ()
                    VarSet _ _ -> fail "Cannot override literal value"
                    Pragma _ s -> evalStatement s   

-- | Provides an optic-free interface to generate a state solely from the environment.
withEnvI :: Environment -> InterpreterState
withEnvI env = (environment .~ env) mempty