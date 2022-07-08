{-# LANGUAGE FunctionalDependencies, OverloadedStrings, DefaultSignatures, DeriveAnyClass, TemplateHaskell #-}

{-|
Module : Epsilon.Internal.Classes
Description : Internal classes and types used in the modules.
Copyright : (c) Thrithralas 2022
License : None
Maintainer : marci.petes@gmail.com
Stability : stable
Description : 
A collection of internal classes a functions used throughout the various compilation modules. Editing anything in this file will most likely cause compilations failures, so be warned!

This file should only be imported in files which implement a new step of compilation.

Those who wish to replace the original `MonadFail` with the one in this module, see the `RebindableSyntax` language extension.
-}
module Epsilon.Internal.Classes where

import Data.Map.Strict ( Map, empty, union )
import Control.Monad ( MonadPlus )
import GHC.Generics ( Generic )
import Data.Store ( Store )
import Optics ( makeLenses )
import Control.Applicative ((<|>)) -- haddock only

import qualified Data.Text as DT
import qualified Prelude as P

import Prelude hiding ( error, MonadFail(..) )

-- | `MonadFail` but for `DT.Text` rather than `String`.
class Monad m => MonadFail m where
    -- | Indicates the failure of a monad with a provided error message. Should be handleable with the `(<|>)` operator.
    fail :: DT.Text -> m a

instance MonadFail IO where
    fail = P.fail . DT.unpack

-- * Program Segments
-- | Flags with can be applied to a function by the semantic analyzer. Use \<# PRAGMAS #\> to apply them.
data FunctionFlags =
    -- | Indicates the function or statement is impure. If the function result would be discared or the function would be memoized by the analyzer, that action is skipped. 
    Impure |
    -- | Indicates that a function or statement should not be inlined. Inlining is an unpredictable tool and this should be used sparingly. Best for big functions or large invocations.
    NoInline 
        deriving (Eq, Show, Generic, Store)

-- | Representation of a function.
data Function = MkFunction {
    -- | Represents the precedence and fixity direction - if the function is an operator, otherwise the field should be `Nothing`.
    _fixity :: Maybe Fixity,
    -- | Represents the flags applied to a function, like impurity or inlining rules.
    _functionFlags :: [FunctionFlags],
    -- | The list of parameters of the function.
    _params :: [Param], 
    -- | The return type of the function.
    _returnType :: EpsilonType,
    -- | If the function is not a builtin function, this is the definition of the function which will be interpreted. Empty list cant be used for builtin functions as it could be confused with an empty function.
    _statements :: Maybe [Statement] 
        } deriving (Show, Eq, Generic, Store)

-- | Represents a primitive type in the programming language.
data EpsilonType = {- | An unbound integer. -} TInt | {- | A boolean. -} TBool | {- | A string of characters. -} TString | {- | Void (or Top). -} TVoid | {- | A Haskell floating point number. -} TFloat | {- | A function. Parameter types and return type undisclosed. -} TFunction deriving (Eq, Show, Generic, Store)

-- | Utility function to drop the head T from the types.
showE :: EpsilonType -> DT.Text
showE = DT.pack . tail . show

-- | Evaluated values represented by the interpreter and primitives parsed.
data Value = {- | An unbound integer using Haskell's Integer type -} VInt Integer | {- | A boolean -} VBool Bool | {- | A String represented as Text from the text package -} VString DT.Text | {- | Void (or Top), commonly refered to as () from Haskell. -} VVoid | {- | A floating point number represented as a Haskell Double -} VFloat Double deriving (Eq, Ord, Show, Generic, Store)

-- | The generic environment throughout the compilation process. All modules pass this state between eachother.
data Environment = MkEnv { -- | A `Map` containing evaluated values and their type representations.
    _valTable :: Map DT.Text (Value, EpsilonType), -- | A `Map` containing functions' representations.
    _functionTable :: Map DT.Text Function 
} deriving (Show, Eq, Generic, Store)


instance Semigroup Environment where
    MkEnv vt ft <> MkEnv vt' ft' = MkEnv (union vt vt') (union ft ft')

instance Monoid Environment where
    mempty = MkEnv empty empty

-- | An expression in the programming language.
data Expression =                         -- | An expression to lookup a value in the lookup table or function table
    Lookup DT.Text                      | -- | An integer literal
    IntLit Integer                      | -- | A boolean literal
    BoolLit Bool                        | -- | A floating point literal
    FloatLit Double                     | -- | A string literal
    StringLit DT.Text                   | -- | A function application. The name of the function is evaluated via the first expression.
    ApplyFun Expression [Expression]
        deriving (Eq, Show, Generic, Store)

-- | A statement of the main program. A collection of statements builds the body of a function, which are evaluated in order.
data Statement =                                -- | Setting the value of a variable to the given expression.
    VarSet Expression Expression              | -- | An if statement without an else branch.
    If Expression [Statement]                 | -- | An if statement with an else branch.
    IfElse Expression [Statement] [Statement] | -- | A while statement.
    While Expression [Statement]              | -- | A return statement. After this is evaluated, no other statements are evaluated in the same function.
    Return Expression                         | -- | Discarding a value. Used when invoking functions with side effects like `print` or `printStr`.
    Action Expression                         | -- | A language pragma wrapping around a statement
    Pragma DT.Text Statement                  | -- | A flag indicating that the enviroment changed here. Used as a reference for pragmas.
    EnvironmentChanged DT.Text  
        deriving (Eq, Show, Generic, Store)

-- | Represents the precedence of an operator and the direction of the fixation.
data Fixity =                                                             -- | An operator which can't be "chained". An example is the `(==)` operator which cannot be used in sequence
    Infix { {- | The precedence of the operator -} getFixity :: Int }   | -- | An operator which chains right. An example is the `(^)` operator which when applied in sequence is parsed like this: "a ^ b ^ c ^ d == a ^ (b ^ (c ^ d))"
    InfixR { {- | The precedence of the operator -} getFixity :: Int }  | -- | An operator which chains left. An example is the `(/)` operator which when applied in sequence is parsed like this: "a \/ b \/ c \/ d == ((a \/ b) \/ c) \/ d"
    InfixL { {- | The precedence of the operator -} getFixity :: Int } 
        deriving (Eq, Show, Generic, Store)

-- | Represents a function parameter.
data Param =              -- | An unnamed, but typed function parameter. Should only be used in a builtin function.
    Unnamed EpsilonType | -- | An untyped, but named parameter. Should not be used in builtin functions.
    Inferred DT.Text    | -- | A parameter with both a name and a type.
    WellTyped EpsilonType DT.Text 
        deriving (Eq, Show, Generic, Store)

-- * The Epsilon Module
-- | Represents a module of compilation. The class expects components to be implemented similarly to a state monad but other methods are also possible. See the individual functions for details.
class (MonadPlus w, Monoid s) => EpsilonModule w s | w -> s where
    -- | Executes the given module on the supplied state. Since the module may produce `IO` actions, the result is wrapped in an `IO` monad. The function returns the resulting environment, a set of error messages and - if it succeeded - the result of the module.
    runModule :: w a {- ^ The module. -} -> s {- ^ The inital state. -} -> IO (Environment, [DT.Text], Maybe a) {- ^ The environment, errors and the result of the module. -}
    -- | Indicates the unhandled failure of the module. If the module implements `MonadFail` it should be the equivalent to `fail`.
    error :: DT.Text -> w a
    -- | A non-fatal warning. 
    warn :: DT.Text -> w ()
    -- | Returns the current state.
    get :: w s
    -- | Sets the state.
    put :: s -> w ()

-- | Modifies the current state based on a function.
modify :: EpsilonModule w s => (s -> s) -> w ()
modify f = get >>= put . f

-- | Alias for the if-then-else statement. Only here for the sake of `RebindableSyntax`.
ifThenElse :: Bool -> a -> a -> a
ifThenElse True a _ = a
ifThenElse False _ b = b

-- * TH Generated Lenses
makeLenses ''Environment
makeLenses ''Function