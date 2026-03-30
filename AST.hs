module AST (Program, Symbol(..), Assignment(..), Expression(..), Type(..)) where

import Data.Map
import Control.Monad.State

data Type = BooleanType | IntegerType | FunctionType Type Type deriving (Show, Eq)
type Program = ([Assignment], Expression)

newtype Symbol = Symbol String deriving (Eq, Ord)
data Assignment = Assignment Symbol Expression deriving Show
data Expression
    = Boolean Bool
    | Integer Int
    | Variable Symbol
    | If Expression Expression Expression
    | Function Symbol Type Expression
    | Application Expression Expression
    deriving Show

instance Show Symbol where
    show (Symbol s) = s
