{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
module Epsilon.Internal.Parser where

import Optics hiding (assign)
import Data.List
import Control.Applicative
import Control.Monad
import Data.Char
import Data.Function

newtype Parser a = Parser { runParser :: ParserState -> Either ParserState (ParserState, a) }

data ParserState = PS {
    _environment :: Environment,
    _backlog :: [String],
    _string :: String,
    _lineNum :: Int,
    _columnNum :: Int,
    _crashed :: Bool
} deriving Show

type Environment = [(String, EnvironmentEntry)]

data EpsilonType = TInt | TBool | TString | TVoid | TFloat | TFunction deriving (Eq, Show)

showE :: EpsilonType -> String
showE = tail . show

data Value = VInt Integer | VBool Bool | VString String | VVoid | VFloat Double | VFunction String EnvironmentEntry deriving Show

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


data EnvironmentEntry =
    Function { _fixity :: Maybe Fixity, _params :: [Param], _returnType :: EpsilonType, _statements :: Maybe [Statement] } |
    Value { _value :: Value, _etype :: EpsilonType }
        deriving Show

data Fixity = Infix { getFixity :: Int} | InfixR { getFixity :: Int } | InfixL { getFixity :: Int } deriving (Eq, Show)
data Param = Unnamed EpsilonType | Inferred String | WellTyped EpsilonType String deriving Show


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
        Left _   -> p2 ps
        Right pa -> Right pa

instance Monad Parser where
    Parser p >>= f = Parser $ \ps -> case p ps of
        Left l -> Left l
        Right (ps', a) -> runParser (f a) ps'


instance MonadFail Parser where
    fail s = Parser $ \st -> Left ((crashed .~ True) . (backlog %~ (format s st :)) $ st)
        where
            format s st = foldr (++) [] [s, " at line ", show (st ^. lineNum), " at column ", show (st ^. columnNum)]

instance MonadPlus Parser where
    mzero = empty
    mplus = (<|>)

instance Semigroup ParserState where
    PS env log s ln cn c <> PS env2 log2 s2 ln2 cn2 c2 = case c of
        False -> PS env log s ln cn c
        True -> PS env2 (union log log2) s2 ln2 cn2 c2

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
    case (ps ^. string) of
        (c:cs) | p c -> c <$ setState ((string .~ cs) . (lineNum %~ lnump) . (columnNum %~ cnump) $ ps)
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

seqOf :: String -> Parser ()
seqOf = mapM_ char

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

stringLit :: Parser String
stringLit = between (char '"') (char '"' <|> fail "Unclosed string") (many $ satisfy  (\c -> case generalCategory c of
        LineSeparator -> False
        _             -> c /= '\"'
        )) <|> fail "Multiline strings not supported"

keywords :: [String]
keywords = ["if", "else", "while", "function", "operator", "builtin", "infixr", "infixl", "infix", "true", "false"]

varName :: Parser String
varName = some (satisfy isAlpha) >>= \s -> s <$ guard (notElem s keywords) <|> fail "Cannot use reserved keyword as variable name"

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
                    o val val' <$ fails op


groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)


hierarchy :: [(Fixity,  Parser (a -> a -> a))] -> {- atom -}  Parser a -> {- l bracket -}  Parser b -> {- r bracket -}  Parser c -> Parser a
hierarchy ops unit lbr rbr = hierarchy' (groupOn (getFixity . fst) $ sortOn (getFixity . fst) ops) (unit <|> between lbr rbr (hierarchy ops unit lbr rbr))
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
ws = many_ $ satisfy isSpace

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
    ops <- getState <&> (\e -> [ (f, (\a b -> ApplyFun (Lookup name) [a, b]) <$ tok (seqOf name) ) | (name, Function { _fixity = Just f, ..}) <- e ^. environment])
    tok $ hierarchy ops (
        FloatLit <$> tok float <|> 
        IntLit <$> tok signedInt <|> 
        StringLit <$> tok stringLit <|>
        BoolLit <$> tok bool <|>
        tok funInvoke <|>
        Lookup <$> tok varName) 
        lbr rbr

funInvoke :: Parser Expression
funInvoke = do
    name <- some $ tok $ satisfy isAlpha
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
    name <- tok $ many $ satisfy (\s -> not (isAlphaNum s) && s /= ')' && s /= '(' && s /= '[' && s /= ']' && s /= '{' && s /= '}' )
    guard (notElem name keywords) <|> fail "Can't use reserved keyword as operator name"
    (params, returnType, body) <- functionSignature $ null b
    let fixity = case j of {
        Just 'l' -> InfixL f;
        Just 'r' -> InfixR f;
        _        -> Infix f;
    }

    EnvironmentChanged <$ modify (over environment ((name, Function (Just fixity) params returnType (case b of { Just _ -> Nothing; Nothing -> Just body; })) :))
    
function :: Parser Statement
function = do
    ws
    b <- optional $ tok $ seqOf "builtin"
    tok $ seqOf "function"
    name <- tok $ many $ satisfy isAlpha
    guard (notElem name keywords) <|> fail "Can't use reserved keyword as function name"   
    (params, returnType, body) <- functionSignature $ null b
    EnvironmentChanged <$ modify (over environment ((name, Function Nothing params returnType (case b of { Just _ -> Nothing; Nothing -> Just body; })) :))

functionSignature :: Bool {- builtin -} -> Parser ([Param], EpsilonType, [Statement])
functionSignature ib = do
    ws
    params <- between (char' '(') (char' ')') $ sepBy1 (do
            ps <- sepBy1 (tok $ many $ satisfy $ isAlpha) (char' ':')
            case ps of
                []           -> fail "Empty parameter in function or operator declaration"
                [x] | ib     -> pure $ Inferred x
                [x] | not ib -> pure $ Unnamed (toType x)
                [x,y]        -> pure $ WellTyped (toType y) x
                _            -> fail "Malformed parameter in function or operator declaration"
        ) (char' ',')
    char' ':'
    t <- toType <$> (tok $ many $ satisfy isAlpha)
    case ib of
        False -> pure $ (params, t, [])
        True -> (\s -> (params, t, s)) <$> between (char' '{') (char' '}') multiS

toType :: String -> EpsilonType
toType "int" = TInt
toType "bool" = TBool
toType "float" = TFloat
toType "string" = TString
toType "void" = TVoid

statement :: Parser Statement
statement = assign <|> while <|> ifElse <|> ifS <|> operator <|> function <|> returnStatement <|> Action <$> expression

returnStatement :: Parser Statement
returnStatement = do
    ws
    tok $ seqOf "return"
    Return <$> tok expression


multiS :: Parser [Statement]
multiS = many statement--sepBy statement (satisfy (\c -> case generalCategory c of { LineSeparator -> True; _ -> False}) <* ws)

eof :: Parser ()
eof = Parser $ \st -> case (st ^. string) of
    [] -> pure (st, ())
    _  -> Left (crashed .~ True $ st)


program :: Parser [Statement]
program = ws *> multiS <* (eof <|> fail "Unparsed program segment")