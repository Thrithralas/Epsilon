{-# LANGUAGE TemplateHaskell, RecordWildCards, OverloadedStrings, RebindableSyntax, DeriveAnyClass #-}

{-|
Module : Epsilon.Internal.Parser
Description : Utilities for parsing text into an abstract syntax tree.
Copyright : (c) Thrithralas 2022
License : None
Maintainer : marci.petes@gmail.com
Stability : experimental
Description : 
The `Parser` is made up of by far the most components and was arguably the hardest part to program. Every parser eventually boils down to the `satisfy` parser which advances one character and updates line and character indices in the state.

Error messages aren't the best as many errors are thrown by simple `satisfy` or `char` parsers which do not specify any error messages.
-}

module Epsilon.Internal.Parser where

import Optics hiding ( assign, uncons )
import Data.List hiding ( tail, uncons, isPrefixOf, insert )
import Control.Monad hiding ( MonadFail(..) )
import Data.Char
import Data.Function
import Data.Text ( Text, pack, uncons, unpack, isPrefixOf )
import Epsilon.Internal.Classes
import Prelude hiding ( MonadFail(..), error )
import qualified Data.Text as T ( empty )
import Data.String
import Control.Applicative
import Data.Map.Strict ( assocs, insert, Map )
-- import Debug.Trace

-- | The parser represented as a state monad.
newtype Parser a = Parser { runParser :: ParserState -> Either ParserState (ParserState, a) }

-- | The internal state of the parser.
data ParserState = PS {
    _environment :: Environment, -- ^ The environment.
    _backlog :: [(Int, Int, Text)], -- ^ Any errors the parser collected, containing line and column numbers.
    _string :: Text, -- ^ The remaining text to parse.
    _lineNum :: Int, -- ^ Current line number.
    _columnNum :: Int, -- ^ Current column number.
    _crashed :: Bool -- ^ Wether the parser crashed or not.
} deriving Show

-- * TH Generated Lenses
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

instance MonadPlus Parser where
    mplus = (<|>)
    mzero = empty

instance EpsilonModule Parser ParserState where
    runModule s st = case runParser s st of
        Left l -> pure (l ^. environment, getBacklog (l ^. backlog), Nothing)
        Right (r,a) -> pure (r ^. environment, [], Just a)
        where
            getBacklog = (:[]) . (\(_,_,b) -> b) . maximumBy orderLogs
            orderLogs (a,b,s) (d,c,s2)
                | a == d && b == c = case isPrefixOf "<" s of
                    True -> LT
                    False -> case isPrefixOf "<" s2 of
                        True -> GT
                        False -> EQ
                | a > d || (a == d && b > c) = GT
                | otherwise = LT
    warn = error
    put ps = Parser $ const $ Right (ps, ())
    get = Parser $ \ps -> Right (ps,ps)
    error tx = Parser $ \st -> Left ((crashed .~ True) . (backlog %~ (:) (format tx st) ) $ st)
        where
            format :: Text -> ParserState -> (Int, Int, Text)
            format s st = (st ^. lineNum, st ^. columnNum, foldr (<>) T.empty [s, " at line ", pack $ show (st ^. lineNum + 1), " at column ", pack $ show (st ^. columnNum + 1)])



instance Monad Parser where
    Parser p >>= f = Parser $ \ps -> case p ps of
        Left l -> Left l
        Right (ps', a) -> runParser (f a) ps'


instance MonadFail Parser where
    fail tx = Parser $ \st -> Left ((crashed .~ True) . (backlog %~ (:) (format tx st) ) $ st)
        where
            format :: Text -> ParserState -> (Int, Int, Text)
            format s st = (st ^. lineNum, st ^. columnNum, foldr (<>) T.empty [s, " at line ", pack $ show (st ^. lineNum + 1), " at column ", pack $ show (st ^. columnNum + 1)])


instance Semigroup ParserState where
    ps1 <> ps2 = over backlog (\l -> union l (ps1 ^. backlog)) $ ps2
    

instance Monoid ParserState where
    mempty = PS mempty [] "" 0 0 False

-- * Building Block Parsers

-- | Checks whether the next character in the sequence matches the predicate and consumes it.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
    ps <- get
    case uncons (ps ^. string) of
        Just (c,cs) | p c -> c <$ put ((string .~ cs) . (lineNum %~ lnump) . (columnNum %~ cnump) $ ps)
            where
                lnump v = case c of
                    '\n'          -> v + 1
                    _             -> v
                cnump v = case c of
                    '\n'          -> 0
                    _             -> v + 1
        _            -> empty

-- | Checks wether the next character in the sequence matches the given character and consumes it.
char :: Char -> Parser ()
char = void . satisfy . (==)

-- | Matches a sequence of characters.
seqOf :: Text -> Parser ()
seqOf = mapM_ char . unpack

-- | Parses an unsigned integer. This parser is greedy and will parse as long as it can.
unsignedInt :: Integral i => Parser i
unsignedInt = foldl1 (\acc a -> 10 * acc + a) 
                <$> some ((\a -> fromInteger $ toInteger $ ord a - ord '0') <$> satisfy isDigit)
                <|> fail "Failed to parse unsigned integer"

-- | Parses an integer with optional negative sign.
signedInt :: Integral i => Parser i
signedInt = do
    j <- optional $ char '-'
    (unsignedInt <&> (case j of
        Just _ -> negate
        _ -> id
        )) <|> fail "Failed to parse signed integer"
    
-- | Parses a signed floating point number.
float :: Fractional r => Parser r
float = do
    s <- optional $ char '-'
    i <- unsignedInt <|> fail "Failed to parse integer part of floating point number"
    let sign = if null s then 1 else -1
    char '.' <|> fail "Missing seperator in floating point number"
    ((* sign) . (+ fromInteger i)  . (/ 10) . foldr1 (\a acc -> a + acc / 10)) 
            <$> some ((\a -> fromInteger $ toInteger $ ord a - ord '0') <$> satisfy isDigit) 
            <|> fail "Incomplete floating point number"

-- | Runs a parser between two other parsers and returns the result of the middle one.
between :: Parser a {- ^ Left parser. -} -> Parser c {- ^ Right parser. -} -> Parser b {- ^ Middle parser. -} -> Parser b {- ^ Result of the middle parser. -}
between l r m = l *> m <* r

-- | Same as `between` but discards the result.
between_ :: Parser a -> Parser c -> Parser b -> Parser ()
between_ l r m = l *> void m <* r

-- | Parses a string literal between quotation marks.
stringLit :: Parser Text
stringLit = between (char '"') (char '"' <|> fail "Unclosed string") (fmap pack $ many $ satisfy  (\c -> case generalCategory c of
        LineSeparator -> False
        _             -> c /= '\"'
        )) <|> fail "Multiline strings not supported"

-- | A list of reserved keywords and symbols.
keywords :: [Text]
keywords = ["if", "else", "while", "function", "operator", "builtin", "infixr", "infixl", "infix", "true", "false", "return", "<#", "#>", "(", ")", "{", "}"]

-- | Parses variable or function name. Fails if the parsed name is anything in the list of reserved `keywords`.
varName :: Parser Text
varName = pack <$> some (satisfy isAlpha) >>= \s -> s <$ guard (notElem s keywords) <|> fail "Cannot use reserved keyword as variable name"

-- * Expression/Statement Parsers

-- | Right chaining operator application. The function parses an atomic element then continues parsing operators and elements, applying the operator's result function as much as it can. Example usage:
--
-- @
-- >>> let op = (^) \<$ char \'^\'
-- >>> let atom = unsignedInt
-- >>> runParser (chainr1 atom op) (withStringAndEnv "2^3^2" mempty)
-- 512
-- @
chainr1 :: Parser a {- ^ Atomic element. -} ->  Parser (a -> a -> a) {- ^ Operator -} ->  Parser a {- ^ Result of chaining. -}
chainr1 v op = v >>= \val ->
    (op >>= \opr ->
        chainr1 v op <&> opr val 
    ) <|> pure val

-- | Same as `chainr1` but chains left instead of right. Example usage:
--
-- @
-- >>> let op = (^) \<$ char \'^\'
-- >>> let atom = unsignedInt
-- >>> runParser (chainl1 atom op) (withStringAndEnv "2^3^2" mempty)
-- 64
-- @
chainl1 :: Parser a {- ^ Atomic element. -} ->  Parser (a -> a -> a) {- ^ Operator -} ->  Parser a {- ^ Result of chaining. -}
chainl1 v op = v >>= parseLeft
    where
        parseLeft val = (op >>= \opr -> 
            v >>= \val2 -> parseLeft (opr val val2)) <|> pure val

-- | Indicates that the given parser will fail. Fails if it doesn't.
fails :: Parser a ->  Parser ()
fails s = optional s >>= \m -> case m of
    Just _ -> empty
    Nothing -> pure ()

-- | Chains an operator with no fixity direction. Attempts to parse an expression with or without operator application, but if it would parse more than 1, it fails. Example usage:
--
-- @
-- >>> let op = (^) \<$ char \'^\'
-- >>> let atom = unsignedInt
-- >>> runParser (chain1 atom op) (withStringAndEnv "2^3^2" mempty)
-- Parse Error
-- >>> runParser (chain1 atom op) (withStringAndEnv "2^3" mempty)
-- 8
-- @
chain1 :: Parser a {- ^ Atomic element. -} ->  Parser (a -> a -> a) {- ^ Operator -} ->  Parser a {- ^ Result of chaining. -}
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


-- | Parser any expression with a defined operator hierarchy. The function sorts the list of operators by fixity then descends down the chain list until it reaches the bottom (the atomic). If the atomic operator fails to parse, it attempts to reparse the entire hierarchy between the given brackets. Do note that if there are operators with different fixity directions on the same precedence level, the parser will fail.
hierarchy :: {- | Operators with their assigned fixity. -} [(Fixity,  Parser (a -> a -> a))] -> {- | Atomic element. -}  Parser a -> {- | Left bracket. -}  Parser b -> {- | Right bracket. -}  Parser c {- | Resulting expression. -} -> Parser a
hierarchy ops u lbrac rbrac = hierarchy' (groupOn (getFixity . fst) $ sortOn (getFixity . fst) ops) (u <|> between lbrac rbrac (hierarchy ops u lbrac rbrac))
    where
        groupOn f = groupBy ((==) `on` f)

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
-- | Parses multiple values seperated by a given parser. Must have at least one value, else the parser will fail.
sepBy1 :: Parser a {- ^ The value parser . -} ->  Parser b {- ^ The seperator -} ->  Parser [a] {- ^ List of parsed values -}
sepBy1 v delim = do
    val <- v
    (delim >> (val:) <$> sepBy v delim) <|> pure [val]

-- | Same as `sepBy` but can parse zero items.
sepBy :: Parser a {- ^ The value parser . -} ->  Parser b {- ^ The seperator -} ->  Parser [a] {- ^ List of parsed values -}
sepBy v delim = sepBy1 v delim <|> pure []

-- | Consumes any white spaces.
ws :: Parser ()
ws = void $ many $ satisfy isSpace

-- | Consumes any whitespaces following the given parser.
tok :: Parser a ->  Parser a
tok = (<* ws)

-- | Parser for the left bracket symbol, eliminating all whitespaces afterwards.
lbr :: Parser ()
lbr = tok $ char '('

-- | Parser for the right bracket symbol, eliminating all whitespaces afterwards.
rbr :: Parser ()
rbr = tok $ char ')'

-- | Tokenized character parser.
char' :: Char -> Parser ()
char' = tok . char

-- | Parses a lowercase boolean literal.
bool :: Parser Bool
bool = True <$ seqOf "true" <|> False <$ seqOf "false"

-- | Parser an expression in Epsilon. The parser collects all in environment operators and their fixities, then attempts to parse a hierarachy. The atomic parsers are all literals or lookups.
expression :: Parser Expression
expression = do
    void $ many $ satisfy (\c -> isSpace c && c /= '\n')
    ops <- get <&> 
        (\e -> 
            [ (fx, (\a b -> ApplyFun (Lookup nam) [a, b]) <$ tok (seqOf nam) ) 
            | (nam, MkFunction { _fixity = (Just fx) }) <- assocs $ e ^. (environment % functionTable)
            ] ) 
    tok $ hierarchy ops (
        FloatLit <$> tok float <|> 
        IntLit <$> tok signedInt <|> 
        StringLit <$> tok stringLit <|>
        BoolLit <$> tok bool <|>
        tok funInvoke <|>
        Lookup <$> tok varName <|> fail "Failed to parse expression") 
        lbr rbr

-- | Parses a function invocation. Function invocations follow the standard c-style syntax.
funInvoke :: Parser Expression
funInvoke = do
    name <- fmap pack $ some $ satisfy isAlpha
    xs <- between lbr rbr $ sepBy expression (char' ',')
    pure $ ApplyFun (Lookup name) xs

-- | Parser an Epsilon pragma. See the language documentation for more details.
pragma :: Parser Statement
pragma = do
    str <- between (tok $ seqOf "<#") (tok $ seqOf "#>") (fmap pack $ some $ tok $ satisfy isUpper)
    st <- statement
    pure $ Pragma str st

-- | Parses an assignment statement.
assign :: Parser Statement
assign = do
    [a,b] <- sepBy1 expression (seqOf ":=")
    pure $ VarSet a b

-- | Parses a while statement, including its entire body.
while :: Parser Statement
while = do
    ws
    tok $ seqOf "while"
    b <- tok expression
    s <- between (char' '{') (char' '}') $ multiS
    pure $ While b s

-- | Parses an if statement with no else branch.
ifS :: Parser Statement
ifS = do
    ws
    tok $ seqOf "if"
    b <- expression
    s <- between (char' '{') (char' '}') $ multiS
    pure $ If b s

-- | Parses and if statement with an else branch.
ifElse :: Parser Statement
ifElse = do
    If b s <- ifS
    ws
    tok $ seqOf "else"
    s' <- between (char' '{') (char' '}') $ multiS
    pure $ IfElse b s s'

-- | Parses an operator declaration. Operators can only contain symbols that are not brackets. For more details see the language documentation.
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
    EnvironmentChanged name <$ modify (over (environment % functionTable) (insert name (MkFunction (Just fx) [] ps rt (case b of { Just _ -> Nothing; Nothing -> Just body; }))))

-- | Parses a function declaration. Function names can only contain letters from the alphabet.
function :: Parser Statement
function = do
    ws
    b <- optional $ tok $ seqOf "builtin"
    tok $ seqOf "function"
    name <- fmap pack $ tok $ many $ satisfy isAlpha
    guard (notElem name keywords) <|> fail "Can't use reserved keyword as function name"   
    (pr, rt, body) <- functionSignature $ null b
    EnvironmentChanged name <$ modify (over (environment % functionTable) (insert name (MkFunction Nothing [] pr rt (case b of { Just _ -> Nothing; Nothing -> Just body; }))))

-- | Parses a uniform function signature, including parameters, return type and body.
functionSignature :: Bool {- ^ Wether the function is builtin or not. -} -> Parser ([Param], EpsilonType, [Statement]) {- ^ Returns the list of `Param`s, the return type and the function body. -}
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

-- | Converts a text into a primitve type in Epsilon.
toType :: Text -> EpsilonType
toType "int" = TInt
toType "bool" = TBool
toType "float" = TFloat
toType "string" = TString
toType "void" = TVoid
toType _ = undefined

-- | Parses a statement in Epsilon.
statement :: Parser Statement
statement = assign <|> while <|> ifElse <|> ifS <|> operator <|> function <|> returnStatement <|> pragma <|> Action <$> expression <|> fail "Failed to parse statement"

-- | Parses a return statement.
returnStatement :: Parser Statement
returnStatement = do
    ws
    tok $ seqOf "return"
    Return <$> tok expression

-- | Parses multiple statements seperated by newlines.
multiS :: Parser [Statement]
multiS = sepBy statement $ many $ tok $ char '\n' --sepBy statement (satisfy (\c -> case generalCategory c of { LineSeparator -> True; _ -> False}) <* ws)

-- | Checks wether there are no more characters to parse. Fails if there are.
eof :: Parser ()
eof = Parser $ \st -> case uncons (st ^. string) of
    Nothing -> pure (st, ())
    _  -> Left (crashed .~ True $ st)

-- | Parses an entire program.
program :: Parser [Statement]
program = ws *> multiS <* eof

-- | Provides an optic-free interface to generate a state solely from the environment and the text to parse.
withStringAndEnv :: Text -> Map Text Function -> ParserState
withStringAndEnv tx env = (string .~ tx) . (environment % functionTable .~ env) $ mempty