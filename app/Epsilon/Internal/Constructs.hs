{-# LANGUAGE TupleSections, 
             AllowAmbiguousTypes,
             ScopedTypeVariables,
             TypeSynonymInstances,
             ViewPatterns,
             TemplateHaskell
#-}

module Epsilon.Internal.Constructs where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Functor
import Data.List hiding ( uncons )
import Data.Function
import Control.Monad.IO.Class
import Optics.TH
import Optics hiding ( uncons )

newtype State m s t = State { runState :: s -> m (s, t) }

get :: Applicative m => State m s s
get = State $ \s -> pure (s,s)

put :: Applicative m => s -> State m s s
put = State . const . pure . join (,)

modify :: Monad m => (s -> s) -> State m s s
modify f = get >>= put . f

instance Functor m => Functor (State m s) where
    fmap f (State st) = State $ \s -> (\(a,b) -> (a, f b)) <$> st s

instance Monad m => Applicative (State m s) where
    pure = State . flip ((pure .) . (,))
    (State stf) <*> (State sta) = State $ \s -> do
        (s', f) <- stf s
        (s'', a) <- sta s'
        pure (s'', f a)

instance Monad m => Monad (State m s) where
    (State st) >>= f = State $ \s -> st s >>= \(s', a) -> runState (f a) s'

instance MonadPlus m => Alternative (State m s) where
    empty = State $ const empty
    (State sta) <|> (State stb) = State $ \s -> sta s <|> stb s

instance MonadPlus m => MonadPlus (State m s) where
    mzero = empty
    mplus = (<|>)

instance MonadIO m => MonadIO (State m s) where
    liftIO io = State $ \s -> (s,) <$> liftIO io

instance MonadFail m => MonadFail (State m s) where
    fail = State . const . fail


instance MonadFail (Either String) where
    fail = Left

type CanFail m = (MonadFail m, MonadPlus m)
type IOContext m = (CanFail m, MonadIO m)

data Expression = 
    Lookup String                    |
    IntLit Integer                   |
    BoolLit Bool                     |
    FloatLit Double                  |
    StringLit String                 |
    ApplyFun Expression [Expression]
        deriving Show

data Statement =
    VarSet Expression Expression              |
    If Expression [Statement]                 |
    IfElse Expression [Statement] [Statement] |
    While Expression [Statement]              |
    Return Expression                         |
    Action Expression                         |
    EnvironmentChanged   
        deriving Show



data Fixity = InfixR { precedence :: Int } | InfixL { precedence :: Int } | Infix { precedence :: Int } deriving (Eq, Show)

data Param = Inferred String | Typed String String | Unnamed String deriving Show

data Value = VInt Integer | VBool Bool | VFloat Double | VString String  | VFunction String EnvironmentEntry | VVoid deriving Show

data EnvironmentEntry =
    Operator { fixity :: Fixity, params :: [Param], returnType :: String, statements :: [Statement] } |
    BuiltInOperator  { fixity :: Fixity, params :: [Param], returnType :: String }                 |
    Function [Param] String [Statement]                                                            |
    BuiltinFunction [Param] String                                                                 |
    Field Value
        deriving Show

type Environment = [(String, EnvironmentEntry)]

data ParserState = PState { _env :: Environment, _backlog :: [String], _string :: String, _lineNum :: Int, _columnNum :: Int } deriving Show
makeLenses ''ParserState

defaultParserState :: ParserState
defaultParserState = PState [] [] [] 0 0

type Parser m t = State m ParserState t


class Stream s t where
    uncons :: s -> Maybe (t, s)

instance Stream String Char where
    uncons (x:xs) = Just (x,xs)
    uncons [] = Nothing

instance Stream ParserState Char where
    uncons (PState env log s lnum cnum) = (\(c,s') -> (c, (PState env log s' (lnum' c) (cnum' c)))) <$> uncons s
        where
            lnum' c = case c of
                (generalCategory -> LineSeparator) -> lnum + 1
                _                                  -> lnum
            cnum' c = case c of
                (generalCategory -> LineSeparator) -> 0
                _                                  -> cnum + 1


optional_ :: Alternative f => f a -> f ()
optional_ = void . optional

satisfy :: (Stream s c, MonadPlus m) => (c -> Bool) -> State m s c
satisfy p = State $ \s -> case uncons s of
    Just (c, cs) | p c -> pure (cs, c)
    _                  -> empty

streamSeq :: forall c s m. (Eq c, Stream s c, MonadPlus m) => s -> State m s ()
streamSeq stm = case uncons stm of
    Just (c :: c,cs) -> eq c *> streamSeq @c cs
    _                -> pure ()

eq :: (Eq c, MonadPlus m, Stream s c) => c -> State m s ()
eq c = void $ satisfy (== c)

any :: (MonadPlus m, Stream s c) => State m s c
any = satisfy $ const True

getS :: Applicative m => Parser m String
getS = _string <$> get

getE :: Applicative m => Parser m Environment
getE = _env <$> get

    
eof :: forall c m s. MonadPlus m => Stream s c => State m s ()
eof = State $ \s -> case uncons s of
    Nothing -> pure (s,())
    Just (_ :: c, _) -> empty


unsignedInt :: (Integral i, CanFail m) => Parser m i
unsignedInt = foldl1 (\acc a -> 10 * acc + a) 
                <$> some ((\a -> fromInteger $ toInteger $ ord a - ord '0') <$> satisfy isDigit)
                <|> fail "Failed to parse unsigned integer"

signedInt :: (Integral i, CanFail m) => Parser m i
signedInt = do
    j <- optional $ eq '-'
    unsignedInt <&> (case j of
        Just _ -> negate
        _ -> id
        )
    

float :: (Fractional r, CanFail m) => Parser m r
float = do
    s <- optional $ eq '-'
    i <- unsignedInt
    let sign = if null s then 1 else -1
    eq '.'
    ((* sign) . (+ fromInteger i)  . (/ 10) . foldr1 (\a acc -> a + acc / 10)) 
            <$> some ((\a -> fromInteger $ toInteger $ ord a - ord '0') <$> satisfy isDigit) 
            <|> fail "Incomplete floating point type"


many_ :: Alternative f => f a -> f ()
many_ = void . many

some_ :: Alternative f => f a -> f ()
some_ = void . some

between :: MonadPlus m => Parser m a -> Parser m c -> Parser m b -> Parser m b
between l r m = l *> m <* r

between_ :: MonadPlus m => Parser m a -> Parser m c -> Parser m b -> Parser m ()
between_ l r m = l *> void m <* r

stringLit :: CanFail m => Parser m String
stringLit = between (eq '"') (eq '"' <|> fail "Unclosed string") (many $ satisfy  (\c -> case generalCategory c of
        LineSeparator -> False
        _             -> c /= '\"'
        )) <|> fail "Multiline strings not supported"

keywords :: [String]
keywords = ["if", "else", "while", "function", "operator", "builtin", "infixr", "infixl", "infix", "true", "false"]

varName :: CanFail m => Parser m String
varName = some (satisfy isAlpha) >>= \s -> s <$ guard (notElem s keywords) <|> fail "Cannot use reserved keyword as variable name"


defaultEnv :: Environment
defaultEnv = [
    ("+", BuiltInOperator (InfixL 6) [Unnamed "int", Unnamed "int"] "int"),
    ("-", BuiltInOperator (InfixL 6) [Unnamed "int", Unnamed "int"] "int"),
    ("*", BuiltInOperator (InfixL 7) [Unnamed "int", Unnamed "int"] "int"),
    ("/", BuiltInOperator (InfixL 7) [Unnamed "int", Unnamed "int"] "int"),
    ("^", BuiltInOperator (InfixR 8) [Unnamed "int", Unnamed "int"] "int"),
    ("==", BuiltInOperator (Infix 4) [Unnamed "int", Unnamed "int"] "int"),
    ("print", BuiltinFunction [Unnamed "int"] "void")
    ]


chainr1 :: MonadPlus m => Parser m a -> Parser m (a -> a -> a) -> Parser m a
chainr1 v op = v >>= \val ->
    (op >>= \opr ->
        chainr1 v op <&> opr val 
    ) <|> pure val

chainl1 :: MonadPlus m => Parser m a -> Parser m (a -> a -> a) -> Parser m a
chainl1 v op = v >>= parseLeft
    where
        parseLeft val = (op >>= \opr -> 
            v >>= \val2 -> parseLeft (opr val val2)) <|> pure val

fails :: MonadPlus m => State m s a -> State m s ()
fails s = optional s >>= \m -> case m of
    Just _ -> empty
    Nothing -> pure ()

chain1 :: MonadPlus m => Parser m a -> Parser m (a -> a -> a) -> Parser m a
chain1 v op = do
    val <- v
    opr <- optional $ op
    case opr of
        Nothing -> pure val
        Just o -> do
            val2 <- optional $ v
            case val2 of
                Nothing -> pure val
                Just val' -> do
                    o val val' <$ fails op




groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)

mapping :: Functor f => [f a] -> [b] -> [f b]
mapping = zipWith (flip (<$))

hierarchy :: CanFail m => [(Fixity, Parser m (a -> a -> a))] -> {- atom -} Parser m a -> {- l bracket -} Parser m b -> {- r bracket -} Parser m c ->Parser m a
hierarchy ops unit lbr rbr = hierarchy' (groupOn (precedence . fst) $ sortOn (precedence . fst) ops) (unit <|> between lbr rbr (hierarchy ops unit lbr rbr))
    where
        hierarchy' :: CanFail m => [[(Fixity, Parser m (a -> a -> a))]] -> Parser m a -> Parser m a
        hierarchy' [] unit = unit
        hierarchy' (os:oss) unit = case precedenceDir (map fst os) of
            Just (InfixR _) -> chainr1 (hierarchy' oss unit) (asum $ map snd os)
            Just (InfixL _) -> chainl1 (hierarchy' oss unit) (asum $ map snd os)
            Just (Infix _)  -> chain1 (hierarchy' oss unit) (asum $ map snd os)
            _ -> fail "Conflicting fixity directions at the same level"
            where
                precedenceDir :: [Fixity] -> Maybe Fixity
                precedenceDir fs = case nub fs of
                    [x] -> Just x
                    _ -> Nothing

sepBy1 :: MonadPlus m => Parser m a -> Parser m b -> Parser m [a]
sepBy1 v delim = do
    val <- v
    (delim >> (val:) <$> sepBy v delim) <|> pure [val]

sepBy :: MonadPlus m => Parser m a -> Parser m b -> Parser m [a]
sepBy v delim = sepBy1 v delim <|> pure []

ws :: MonadPlus m => Parser m ()
ws = many_ $ satisfy isSpace

tok :: MonadPlus m => Parser m a -> Parser m a
tok = (<* ws)

noEnv :: Functor f => f ((a, b), c) -> f (c, b)
noEnv = fmap (\((_,b),c) -> (c, b))
