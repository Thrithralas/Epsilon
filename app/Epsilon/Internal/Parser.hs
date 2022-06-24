{-# LANGUAGE TemplateHaskell, RecordWildCards, OverloadedStrings, RebindableSyntax, DeriveAnyClass #-}

module Epsilon.Internal.Parser where

import Optics hiding ( assign, uncons )
import Data.List hiding ( tail, uncons )
import Control.Applicative
import Control.Monad hiding ( MonadFail(..) )
import Data.Char
import Data.Function
import Data.Text ( Text, pack, uncons, unpack )
import Epsilon.Internal.Classes
import Prelude hiding ( MonadFail(..) )
import qualified Data.Text as T ( empty )
import Data.String
-- import Debug.Trace
import Data.Store
import GHC.Generics hiding ( Fixity, Infix )

ifThenElse :: Bool -> a -> a -> a
ifThenElse True a _ = a
ifThenElse False _ b = b

newtype Parser a = Parser { runParser :: ParserState -> Either ParserState (ParserState, a) }

data ParserState = PS {
    _environment :: Environment,
    _backlog :: [BacklogEntry],
    _string :: Text,
    _lineNum :: Int,
    _columnNum :: Int,
    _crashed :: Bool
} deriving Show

type Environment = [(Text, EnvironmentEntry)]
type BacklogEntry = (Int, Int, Text)

data EpsilonType = TInt | TBool | TString | TVoid | TFloat | TFunction deriving (Eq, Show, Generic, Store)

showE :: EpsilonType -> Text
showE = pack . tail . show

data Value = VInt Integer | VBool Bool | VString Text | VVoid | VFloat Double | VFunction Text EnvironmentEntry deriving (Show, Generic, Store)

data Expression = 
    Lookup Text                         |
    IntLit Integer                      |
    BoolLit Bool                        |
    FloatLit Double                     |
    StringLit Text                      |
    ApplyFun Expression [Expression]
        deriving (Show, Generic, Store)

data Statement =
    VarSet Expression Expression              |
    If Expression [Statement]                 |
    IfElse Expression [Statement] [Statement] |
    While Expression [Statement]              |
    Return Expression                         |
    Action Expression                         |
    EnvironmentChanged   
        deriving (Show, Generic, Store)


data EnvironmentEntry =
    Function { _fixity :: Maybe Fixity, _params :: [Param], _returnType :: EpsilonType, _statements :: Maybe [Statement] } |
    Value { _value :: Value, _etype :: EpsilonType }
        deriving (Show, Generic, Store)

data Fixity = Infix { getFixity :: Int} | InfixR { getFixity :: Int } | InfixL { getFixity :: Int } deriving (Eq, Show, Generic, Store)
data Param = Unnamed EpsilonType | Inferred Text | WellTyped EpsilonType Text deriving (Show, Generic, Store)


makeLenses ''EnvironmentEntry
makeLenses ''ParserState

instance Functor Parser where
    fmap f (Parser p) = Parser $ fmap (over _2 f) . p

instance Applicative Parser where
    pure a = Parser $ \s -> pure (s,a)
    Parser p1 <*> Parser p2 = Parser $ \ps -> case p1 ps of
        Right (ps', f) -> (over _2 f) <$> p2 ps'
        Left l         -> Left l

instance Alternative Parser where
    empty = fail "<Undisclosed Parse Error>"
    Parser p1 <|> Parser p2 = Parser $ \ps -> case p1 ps of
        Left l1   -> case p2 ps of
            Left l2 -> Left $ l1 <> l2
            Right pr -> Right $ over _1 (l1 <>) pr
        Right pa -> Right pa

instance Monad Parser where
    Parser p >>= f = Parser $ \ps -> case p ps of
        Left l -> Left l
        Right (ps', a) -> runParser (f a) ps'


instance MonadFail Parser where
    fail tx = Parser $ \st -> Left ((crashed .~ True) . (backlog %~ (:) (format tx st) ) $ st)
        where
            format :: Text -> ParserState -> (Int, Int, Text)
            format s st = (st ^. lineNum, st ^. columnNum, foldr (<>) T.empty [s, " at line ", pack $ show (st ^. lineNum + 1), " at column ", pack $ show (st ^. columnNum + 1)])

instance MonadPlus Parser where
    mzero = empty
    mplus = (<|>)

instance Semigroup ParserState where
    ps1 <> ps2 = over backlog (\l -> union l (ps1 ^. backlog)) $ ps2
    

instance Monoid ParserState where
    mempty = PS [] [] "" 0 0 False

getState :: Parser ParserState
getState = Parser $ \ps -> Right (ps,ps)

setState :: ParserState -> Parser ()
setState ps = Parser $ const $ Right (ps, ())

modify :: (ParserState -> ParserState) -> Parser ()
modify f = getState >>= setState . f

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
    ps <- getState
    case uncons (ps ^. string) of
        Just (c,cs) | p c -> c <$ setState ((string .~ cs) . (lineNum %~ lnump) . (columnNum %~ cnump) $ ps)
            where
                lnump v = case c of
                    '\n'          -> v + 1
                    _             -> v
                cnump v = case c of
                    '\n'          -> 0
                    _             -> v + 1
        _            -> empty

char :: Char -> Parser ()
char = void . satisfy . (==)

seqOf :: Text -> Parser ()
seqOf = mapM_ char . unpack

unsignedInt :: Integral i => Parser i
unsignedInt = foldl1 (\acc a -> 10 * acc + a) 
                <$> some ((\a -> fromInteger $ toInteger $ ord a - ord '0') <$> satisfy isDigit)
                <|> fail "Failed to parse unsigned integer"


signedInt :: Integral i => Parser i
signedInt = do
    j <- optional $ char '-'
    (unsignedInt <&> (case j of
        Just _ -> negate
        _ -> id
        )) <|> fail "Failed to parse signed integer"
    

float :: Fractional r => Parser r
float = do
    s <- optional $ char '-'
    i <- unsignedInt <|> fail "Failed to parse integer part of floating point number"
    let sign = if null s then 1 else -1
    char '.' <|> fail "Missing seperator in floating point number"
    ((* sign) . (+ fromInteger i)  . (/ 10) . foldr1 (\a acc -> a + acc / 10)) 
            <$> some ((\a -> fromInteger $ toInteger $ ord a - ord '0') <$> satisfy isDigit) 
            <|> fail "Incomplete floating point number"


many_ :: Alternative f => f a -> f ()
many_ = void . many

some_ :: Alternative f => f a -> f ()
some_ = void . some

between :: Parser a -> Parser c -> Parser b -> Parser b
between l r m = l *> m <* r

between_ :: Parser a -> Parser c -> Parser b -> Parser ()
between_ l r m = l *> void m <* r

stringLit :: Parser Text
stringLit = between (char '"') (char '"' <|> fail "Unclosed string") (fmap pack $ many $ satisfy  (\c -> case generalCategory c of
        LineSeparator -> False
        _             -> c /= '\"'
        )) <|> fail "Multiline strings not supported"

keywords :: [Text]
keywords = ["if", "else", "while", "function", "operator", "builtin", "infixr", "infixl", "infix", "true", "false"]

varName :: Parser Text
varName = pack <$> some (satisfy isAlpha) >>= \s -> s <$ guard (notElem s keywords) <|> fail "Cannot use reserved keyword as variable name"

chainr1 :: Parser a ->  Parser (a -> a -> a) ->  Parser a
chainr1 v op = v >>= \val ->
    (op >>= \opr ->
        chainr1 v op <&> opr val 
    ) <|> pure val

chainl1 :: Parser a ->  Parser (a -> a -> a) ->  Parser a
chainl1 v op = v >>= parseLeft
    where
        parseLeft val = (op >>= \opr -> 
            v >>= \val2 -> parseLeft (opr val val2)) <|> pure val

fails :: Parser a ->  Parser ()
fails s = optional s >>= \m -> case m of
    Just _ -> empty
    Nothing -> pure ()

chain1 :: Parser a ->  Parser (a -> a -> a) ->  Parser a
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
                    o val val' <$ fails op <|> fail "Cannot chain infix operators"


groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)


hierarchy :: [(Fixity,  Parser (a -> a -> a))] -> {- atom -}  Parser a -> {- l bracket -}  Parser b -> {- r bracket -}  Parser c -> Parser a
hierarchy ops u lbrac rbrac = hierarchy' (groupOn (getFixity . fst) $ sortOn (getFixity . fst) ops) (u <|> between lbr rbr (hierarchy ops u lbrac rbrac))
    where
        hierarchy' :: [[(Fixity,  Parser (a -> a -> a))]] ->  Parser a ->  Parser a
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

sepBy1 ::   Parser a ->  Parser b ->  Parser [a]
sepBy1 v delim = do
    val <- v
    (delim >> (val:) <$> sepBy v delim) <|> pure [val]

sepBy ::   Parser a ->  Parser b ->  Parser [a]
sepBy v delim = sepBy1 v delim <|> pure []

ws ::   Parser ()
ws = many_ $ satisfy (\c -> isSpace c && c /= '\n')

tok ::   Parser a ->  Parser a
tok = (<* ws)

lbr :: Parser ()
lbr = tok $ char '('

rbr :: Parser ()
rbr = tok $ char ')'

char' :: Char -> Parser ()
char' = tok . char

bool :: Parser Bool
bool = True <$ seqOf "true" <|> False <$ seqOf "false"

expression :: Parser Expression
expression = do
    ws
    ops <- getState <&> (\e -> [ (f, (\a b -> ApplyFun (Lookup name) [a, b]) <$ tok (seqOf name) ) | (name, Function { _fixity = Just f }) <- e ^. environment])
    tok $ hierarchy ops (
        FloatLit <$> tok float <|> 
        IntLit <$> tok signedInt <|> 
        StringLit <$> tok stringLit <|>
        BoolLit <$> tok bool <|>
        tok funInvoke <|>
        Lookup <$> tok varName <|> fail "Failed to parse expression") 
        lbr rbr

funInvoke :: Parser Expression
funInvoke = do
    name <- fmap pack $ some $ tok $ satisfy isAlpha
    xs <- between lbr rbr $ sepBy expression (char' ',')
    pure $ ApplyFun (Lookup name) xs

assign :: Parser Statement
assign = do
    [a,b] <- sepBy1 expression (seqOf ":=")
    pure $ VarSet a b

while :: Parser Statement
while = do
    ws
    tok $ seqOf "while"
    b <- expression
    s <- between (char' '{') (char' '}') $ multiS
    pure $ While b s

ifS :: Parser Statement
ifS = do
    ws
    tok $ seqOf "if"
    b <- expression
    s <- between (char' '{') (char' '}') $ multiS
    pure $ If b s

ifElse :: Parser Statement
ifElse = do
    If b s <- ifS
    ws
    tok $ seqOf "else"
    s' <- between (char' '{') (char' '}') $ multiS
    pure $ IfElse b s s'


operator :: Parser Statement
operator = do
    ws
    b <- optional $ tok $ seqOf "builtin"
    tok $ seqOf "operator"
    seqOf "infix"
    j <- optional $ satisfy (\a -> a == 'r' || a == 'l')
    ws
    f <- tok unsignedInt
    name <- fmap pack $ tok $ many $ satisfy (\s -> not (isAlphaNum s) && s /= ')' && s /= '(' && s /= '[' && s /= ']' && s /= '{' && s /= '}' )
    guard (notElem name keywords) <|> fail "Can't use reserved keyword as operator name"
    (ps, rt, body) <- functionSignature $ null b
    let fx = case j of {
        Just 'l' -> InfixL f;
        Just 'r' -> InfixR f;
        _        -> Infix f;
    }

    EnvironmentChanged <$ modify (over environment ((name, Function (Just fx) ps rt (case b of { Just _ -> Nothing; Nothing -> Just body; })) :))
    
function :: Parser Statement
function = do
    ws
    b <- optional $ tok $ seqOf "builtin"
    tok $ seqOf "function"
    name <- fmap pack $ tok $ many $ satisfy isAlpha
    guard (notElem name keywords) <|> fail "Can't use reserved keyword as function name"   
    (pr, rt, body) <- functionSignature $ null b
    EnvironmentChanged <$ modify (over environment ((name, Function Nothing pr rt (case b of { Just _ -> Nothing; Nothing -> Just body; })) :))

functionSignature :: Bool {- builtin -} -> Parser ([Param], EpsilonType, [Statement])
functionSignature ib = do
    ws
    pr <- between (char' '(') (char' ')') $ sepBy1 (do
            ps <- sepBy1 (fmap pack $ tok $ many $ satisfy $ isAlpha) (char' ':')
            case ps of
                []           -> fail "Empty parameter in function or operator declaration"
                [x] | ib     -> pure $ Inferred x
                [x] | not ib -> pure $ Unnamed (toType x)
                [x,y]        -> pure $ WellTyped (toType y) x
                _            -> fail "Malformed parameter in function or operator declaration"
        ) (char' ',')
    char' ':'
    t <- toType <$> (fmap pack $ tok $ many $ satisfy isAlpha)
    case ib of
        False -> pure $ (pr, t, [])
        True -> (\s -> (pr, t, s)) <$> between (char' '{') (char' '}') multiS

toType :: Text -> EpsilonType
toType "int" = TInt
toType "bool" = TBool
toType "float" = TFloat
toType "string" = TString
toType "void" = TVoid
toType _ = undefined

statement :: Parser Statement
statement = assign <|> while <|> ifElse <|> ifS <|> operator <|> function <|> returnStatement <|> Action <$> expression <|> fail "Failed to parse statement"

returnStatement :: Parser Statement
returnStatement = do
    ws
    tok $ seqOf "return"
    Return <$> tok expression


multiS :: Parser [Statement]
multiS = sepBy statement $ char '\n' --sepBy statement (satisfy (\c -> case generalCategory c of { LineSeparator -> True; _ -> False}) <* ws)

eof :: Parser ()
eof = Parser $ \st -> case uncons (st ^. string) of
    Nothing -> pure (st, ())
    _  -> Left (crashed .~ True $ st)


program :: Parser [Statement]
program = ws *> multiS <* eof