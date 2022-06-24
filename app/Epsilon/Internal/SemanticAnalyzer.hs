{-# LANGUAGE OverloadedStrings, TemplateHaskell, LambdaCase, DeriveAnyClass #-}
module Epsilon.Internal.SemanticAnalyzer where

import Control.Applicative
import Optics
import Epsilon.Internal.Classes
import Epsilon.Internal.Parser hiding ( environment, modify )
import Control.Monad hiding ( MonadFail(..) )
import Prelude hiding ( MonadFail(..), exp )
import Data.Text ( Text )
-- import Debug.Trace
import Data.List
import Data.Store
import Data.Function
import GHC.Generics

newtype SemanticAnalyzer a = SemanticAnalyzer { runAnalyzer :: AnalyzerState -> Either AnalyzerState (AnalyzerState, a )}

data Severity = Note | Warn | Err deriving (Generic, Store)
data Handling = Ignore | Warning | Error | TryFix

instance Show Severity where
    show Note = "Note"
    show Warn = "Warning"
    show Err = "Error"

type TypeTable = [(Text, (Bool, EpsilonType))]

data AnalyzerState = AS {
    _issues :: [(Severity, Text)],
    _environment :: Environment,
    _typeTable :: TypeTable,
    _currReturnType :: EpsilonType
} deriving (Show, Generic, Store)

makeLenses ''AnalyzerState

instance Semigroup AnalyzerState where
    AS i _ _ _ <> AS i2 e2 t2 c2 = AS (i ++ i2) e2 t2 c2

instance Monoid AnalyzerState where
    mempty = AS [] [] [] TVoid


get :: SemanticAnalyzer AnalyzerState
get = SemanticAnalyzer $ \st -> Right (st,st)

put :: AnalyzerState -> SemanticAnalyzer ()
put st = SemanticAnalyzer $ const $ Right (st, ())

warn :: Text -> SemanticAnalyzer ()
warn tx = SemanticAnalyzer $ \st -> Right $ (over issues ((Warn, format tx st) :) $ st, ())
        where
            format s _ = s

instance Functor SemanticAnalyzer where
    fmap f (SemanticAnalyzer p) = SemanticAnalyzer $ fmap (over _2 f) . p

instance Applicative SemanticAnalyzer where
    pure a = SemanticAnalyzer $ \s -> pure (s,a)
    SemanticAnalyzer p1 <*> SemanticAnalyzer p2 = SemanticAnalyzer $ \ps -> case p1 ps of
        Right (ps', f) -> (over _2 f) <$> p2 ps'
        Left l         -> Left l

instance Alternative SemanticAnalyzer where
    empty = fail "<Undisclosed Analyzer Error>"
    SemanticAnalyzer p1 <|> SemanticAnalyzer p2 = SemanticAnalyzer $ \ps -> case p1 ps of
        Left l1  -> p2 (ps <> l1)
        Right pa -> Right pa

instance Monad SemanticAnalyzer where
    SemanticAnalyzer p >>= f = SemanticAnalyzer $ \ps -> case p ps of
        Left l -> Left l
        Right (ps', a) -> runAnalyzer (f a) ps'

instance MonadPlus SemanticAnalyzer where
    mzero = empty
    mplus = (<|>)

instance MonadFail SemanticAnalyzer where
    fail tx = SemanticAnalyzer $ \st -> Left $ over issues ((Err, format tx st) :) $ st
        where
            format s _ = s



modify :: (AnalyzerState -> AnalyzerState) -> SemanticAnalyzer ()
modify f = get >>= put . f

typeOf :: Expression -> SemanticAnalyzer EpsilonType
typeOf (IntLit _)  = pure TInt
typeOf (FloatLit _) = pure TFloat
typeOf (StringLit _) = pure TString
typeOf (BoolLit _) = pure TBool
typeOf (Lookup s) = do
    st <- get
    case lookup s (st ^. typeTable) of
        Nothing -> fail $ "Variable not in scope: " <> s
        Just (_,t) -> t <$ modify (over typeTable ((s, (True, t)) :))
typeOf (ApplyFun (Lookup s) exps) = do
    st <- get
    case lookup s (st ^. environment) of
        Nothing -> fail $ "Function not in scope: " <> s
        Just (Function _ prs t _)  -> t <$ (checkForMatch prs exps)
            where
                checkForMatch [] [] = pure ()
                checkForMatch (_:_) [] = fail $ "Not enough supplied parameters in function: " <> s
                checkForMatch [] (_:_) = fail $ "Too many supplied parameters in function: " <> s
                checkForMatch (p:ps) (x:xs) = do
                    t'' <- typeOf x
                    case p of
                        Unnamed t' -> guard (t'' == t') <|> fail ("Couldn't match expected type " <> showE t' <> " with actual type " <> showE t'')
                        WellTyped t' n -> guard (t'' == t') <|> fail ("Couldn't match expected type " <> showE t' <> " with actual type " <> showE t'' <> " of parameter '" <> n <> "'")
                        Inferred n -> warn $ "Can't typecheck inferred parameter at compile time of parameter '" <> n <> "'"
                    checkForMatch ps xs
        _ -> undefined
typeOf _ = undefined



analyze :: [Statement] -> SemanticAnalyzer [Statement]
analyze [] = pure []
analyze (s:ss) = case s of
    EnvironmentChanged -> analyze ss
    If exp brnch -> do
        case brnch of 
            [] -> warn "Empty 'if' branch will be omitted" *> analyze ss
            _  -> do
                    t <- typeOf exp
                    case t of
                        TBool -> do
                            ss' <- analyze brnch
                            (If exp ss' :) <$> analyze ss
                        _     -> fail "Non-bool expression in 'if' statement"
    IfElse exp brnch1 brnch2 -> do
        case (brnch1, brnch2) of
            ([],[]) -> warn "Empty 'if else' branches will be omitted" *> analyze ss
            (xs, []) -> warn "Empty 'else' branch will be omitted" *> analyze (If exp xs : ss)
            _ -> do
                when (null brnch1) $ warn "Empty 'if' branch in 'if else' will NOT be omitted. Please import the 'not' function and perform this action manually" 
                t <- typeOf exp
                case t of
                    TBool -> do
                        ss' <- analyze brnch1
                        ss'' <- analyze brnch2
                        (IfElse exp ss' ss'' :) <$> analyze ss
                    _ -> fail "Non-bool expression in 'if else' statement"
    While exp brnch -> do
        case brnch of
            [] -> warn "Empty 'while' branch will be omitted" *> analyze ss
            _  -> do
                    t <- typeOf exp
                    case t of
                        TBool -> do
                            ss' <- analyze brnch
                            (While exp ss' :) <$> analyze ss
                        _ -> fail "Non-bool expression in 'while' statement"
    Action exp -> do
        t <- typeOf exp
        case t of
            TVoid -> (Action exp :) <$> analyze ss
            _     -> warn "Discarding a non-void type will be omitted" *> analyze ss
    Return exp -> do
        t <- typeOf exp
        t' <- (^. currReturnType) <$> get
        guard (t == t') <|> fail ("Couldn't match return type " <> showE t' <> " with actual type " <> showE t)
        case ss of
            [] -> pure []
            _  -> [] <$ warn "Excess statements after return will be omitted"
    VarSet (Lookup str) exp -> do
        st <- get
        t <- typeOf exp
        case lookup str (st ^. typeTable) of
            Nothing -> modify (over typeTable $ (:) (str, (False, t)) )
            Just (_, t') -> guard (t == t') <|> fail ("Couldn't match type " <> showE t' <> " with type " <> showE t <> " of variable '" <> str <> "'")
        (VarSet (Lookup str) exp :) <$> analyze ss
    _ -> undefined

    
lifetimeCheck :: SemanticAnalyzer ()
lifetimeCheck = do
    tt <- (^. typeTable) <$> get
    forM_ (nubBy ((==) `on` fst) $ reverse $ sortOn snd $ map (\(a,(b,_)) -> (a,b)) tt) (\(a,b) -> unless b $ warn $ "Unused variable '" <> a <> "'")

analyzeProgramm :: [Statement] -> SemanticAnalyzer [Statement]
analyzeProgramm st = analyze st <* lifetimeCheck