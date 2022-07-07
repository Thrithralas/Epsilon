{-# LANGUAGE FunctionalDependencies, OverloadedStrings, DefaultSignatures, DeriveAnyClass, TemplateHaskell #-}
module Epsilon.Internal.Classes where

import Data.Text hiding ( tail, foldr, empty )
import Prelude hiding ( error, MonadFail(..) )
import qualified Prelude as P
import GHC.Generics ( Generic )
import Data.Store
import Control.Monad ( MonadPlus )
import Data.Map.Strict as Map
import Optics

class Monad m => MonadFail m where
    fail :: Text -> m a

instance MonadFail IO where
    fail = P.fail . unpack

type BacklogEntry = (Int, Int, Text)

data FunctionFlags = Impure | NoInline deriving (Eq, Show, Generic, Store)

data Function = MkFunction { _fixity :: Maybe Fixity, _functionFlags :: [FunctionFlags], _params :: [Param], _returnType :: EpsilonType, _statements :: Maybe [Statement] } deriving (Show, Eq, Generic, Store)

data EpsilonType = TInt | TBool | TString | TVoid | TFloat | TFunction deriving (Eq, Show, Generic, Store)

showE :: EpsilonType -> Text
showE = pack . tail . show

data Value = VInt Integer | VBool Bool | VString Text | VVoid | VFloat Double deriving (Eq, Ord, Show, Generic, Store)

data Environment = MkEnv {
    _valTable :: Map Text (Value, EpsilonType),
    _functionTable :: Map Text Function
} deriving (Show, Eq, Generic, Store)


instance Semigroup Environment where
    MkEnv vt ft <> MkEnv vt' ft' = MkEnv (union vt vt') (union ft ft')

instance Monoid Environment where
    mempty = MkEnv empty empty


data Expression = 
    Lookup Text                         |
    IntLit Integer                      |
    BoolLit Bool                        |
    FloatLit Double                     |
    StringLit Text                      |
    ApplyFun Expression [Expression]
        deriving (Eq, Show, Generic, Store)

data Statement =
    VarSet Expression Expression              |
    If Expression [Statement]                 |
    IfElse Expression [Statement] [Statement] |
    While Expression [Statement]              |
    Return Expression                         |
    Action Expression                         |
    Pragma Text Statement                     |
    EnvironmentChanged   
        deriving (Eq, Show, Generic, Store)


data Fixity = Infix { getFixity :: Int} | InfixR { getFixity :: Int } | InfixL { getFixity :: Int } deriving (Eq, Show, Generic, Store)
data Param = Unnamed EpsilonType | Inferred Text | WellTyped EpsilonType Text deriving (Eq, Show, Generic, Store)


class (MonadPlus w, Monoid s) => EpsilonModule w s m | w -> s m where
    runModule :: w a -> s -> IO (Environment, [Text], Maybe a)
    ctor :: (s -> m (s, a)) -> w a
    error :: Text -> w a
    warn :: Text -> w ()
    get :: w s
    put :: s -> w ()


ifThenElse :: Bool -> a -> a -> a
ifThenElse True a _ = a
ifThenElse False _ b = b

modify :: EpsilonModule w s m => (s -> s) -> w ()
modify f = get >>= put . f

makeLenses ''Environment