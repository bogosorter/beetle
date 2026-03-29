module AST (Program, Environment, Symbol(..), Assignment(..), Expression(..)) where

import Data.Map
import Control.Monad.State

type Program = ([Assignment], Expression)
type Environment = Map Symbol Expression

newtype Symbol = Symbol String deriving (Eq, Ord)
data Assignment = Assignment Symbol Expression deriving Show
data Expression
    = Boolean Bool
    | Integer Int
    | Variable Symbol
    | If Expression Expression Expression
    | Function Symbol Expression
    | Application Expression Expression
    deriving Show

instance Show Symbol where
    show (Symbol s) = s
