module Epsilon.Parser where

import Epsilon.Internal.Constructs
import Control.Monad
import Data.Functor
import Data.Char
import Control.Applicative
import Optics hiding ( assign )

isOperator :: EnvironmentEntry -> Bool
isOperator (Operator _ _ _ _) = True
isOperator (BuiltInOperator _ _ _) = True
isOperator _ = False

streamSeqEnv :: MonadPlus m => String -> Parser m ()
streamSeqEnv s = streamSeq @Char $ PState [] [] s 0 0


bool :: CanFail m => Parser m Bool
bool = True <$ streamSeqEnv "true" <|> False <$ streamSeqEnv "false"

expression :: CanFail m => Parser m Expression
expression = do
    ws
    ops <- getE <&> (\e -> [ (fixity entry, (\a b -> ApplyFun (Lookup name) [a, b]) <$ tok (streamSeqEnv name) ) | (name, entry) <- e, isOperator entry])
    tok $ hierarchy ops (
        FloatLit <$> tok float <|> 
        IntLit <$> tok signedInt <|> 
        StringLit <$> tok stringLit <|>
        BoolLit <$> tok bool <|>
        tok funInvoke <|>
        Lookup <$> tok varName) 
        (tok $ eq '(') (tok $ eq ')')

funInvoke :: CanFail m => Parser m Expression
funInvoke = do
    name <- some $ tok $ satisfy isAlpha
    xs <- between (tok $ eq '(') (tok $ eq ')') $ sepBy expression (tok $ eq ',')
    pure $ ApplyFun (Lookup name) xs

assign :: CanFail m => Parser m Statement
assign = do
    [a,b] <- sepBy1 expression (streamSeqEnv ":=")
    pure $ VarSet a b

while :: CanFail m => Parser m Statement
while = do
    ws
    tok $ streamSeqEnv "while"
    b <- expression
    s <- between (tok $ eq '{') (tok $ eq '}') $ multiS
    pure $ While b s

ifS :: CanFail m => Parser m Statement
ifS = do
    ws
    tok $ streamSeqEnv "if"
    b <- expression
    s <- between (tok $ eq '{') (tok $ eq '}') $ multiS
    pure $ If b s

ifElse :: CanFail m => Parser m Statement
ifElse = do
    If b s <- ifS
    ws
    tok $ streamSeqEnv "else"
    s' <- between (tok $ eq '{') (tok $ eq '}') $ multiS
    pure $ IfElse b s s'

operator :: CanFail m => Parser m Statement
operator = do
    ws
    b <- optional $ tok $ streamSeqEnv "builtin"
    tok $ streamSeqEnv "operator"
    streamSeqEnv "infix"
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
    let op = case b of {
        Just _ -> BuiltInOperator fixity params returnType;
        _      -> Operator fixity params returnType body;
    }
    EnvironmentChanged <$ modify (over env ((name, op) :))
    
function :: CanFail m => Parser m Statement
function = do
    ws
    b <- optional $ tok $ streamSeqEnv "builtin"
    tok $ streamSeqEnv "function"
    name <- tok $ many $ satisfy isAlpha
    guard (notElem name keywords) <|> fail "Can't use reserved keyword as function name"   
    (params, returnType, body) <- functionSignature $ null b
    let func = case b of {
        Just _ -> BuiltinFunction params returnType;
        _      -> Function params returnType body ;
    }
    EnvironmentChanged <$ modify (over env ((name, func) :))

functionSignature :: CanFail m => Bool {- builtin -} -> Parser m ([Param], String, [Statement])
functionSignature ib = do
    ws
    params <- between (tok $ eq '(') (tok $ eq ')') $ sepBy1 (do
            ps <- sepBy1 (tok $ many $ satisfy $ isAlpha) (tok $ eq ':')
            case ps of
                []           -> fail "Empty parameter in function or operator declaration"
                [x] | ib     -> pure $ Inferred x
                [x] | not ib -> pure $ Unnamed x
                [x,y]        -> pure $ Typed x y
                _            -> fail "Malformed parameter in function or operator declaration"
        ) (tok $ eq ',')
    tok $ eq ':'
    t <- tok $ many $ satisfy isAlpha
    case ib of
        False -> pure $ (params, t, [])
        True -> (\s -> (params, t, s)) <$> between (tok $ eq '{') (tok $ eq '}') multiS



statement :: CanFail m => Parser m Statement
statement = assign <|> while <|> ifElse <|> ifS <|> operator <|> function <|> returnStatement <|> Action <$> expression

returnStatement :: CanFail m => Parser m Statement
returnStatement = do
    ws
    tok $ streamSeqEnv "return"
    Return <$> tok expression


multiS :: CanFail m => Parser m [Statement]
multiS = many statement--sepBy statement (satisfy (\c -> case generalCategory c of { LineSeparator -> True; _ -> False}) <* ws)

program :: CanFail m => Parser m [Statement]
program = ws *> multiS <* eof @Char