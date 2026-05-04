{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Closures where

import Debug.Trace

type Program = ([FunctionDefinition], Expression)

-- A function is defined by a name, an argument type, a return type, a closure and an expression
data FunctionDefinition = FunctionDefinition String Type Type ClosureType Expression | BuiltInFunction String Type Type
    deriving Show
data Expression
    = Boolean Bool
    | Integer Int
    | Argument Type -- The argument of the current function (invalid if we are in the global scope)
    | Variable Int Type -- The index of the variable in the closure
    | Local String Type -- A Variable that was declared with let
    | If Expression Expression Expression
    | Closure FunctionDefinition [Expression]
    | Application Expression Expression
    | Let String Expression Expression
    deriving Show

data Type = BooleanType | IntegerType | ClosureType Type Type
    deriving Show
type ClosureType = [Type]

typeOf :: Expression -> Type
typeOf (Boolean _) = BooleanType
typeOf (Integer _) = IntegerType
typeOf (Argument t) = t
typeOf (Variable _ t) = t
typeOf (Local _ t) = t
typeOf (If _ expression _) = typeOf expression
typeOf (Closure function _) = case function of
    (FunctionDefinition _ argumentType returnType _ _) -> ClosureType argumentType returnType
    (BuiltInFunction _ argumentType returnType) -> ClosureType argumentType returnType
typeOf (Application closure _) = returnType
    where (ClosureType argumentType returnType) = typeOf closure
typeOf (Let _ _ expression) = typeOf expression
