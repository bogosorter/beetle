{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Closures where

import qualified AST
import qualified TypeChecker

type Program = ([FunctionDefinition], Expression)

-- A function is defined by a name, an argument type, a return type, a closure and an expression
data FunctionDefinition = FunctionDefinition String Type Type ClosureType Expression | BuiltInFunction String
data Expression
    = Boolean Bool
    | Integer Int
    | Argument Type -- The argument of the current function (invalid if we are in the global scope)
    | Variable Int Type -- The index of the variable in the closure
    | If Expression Expression Expression
    | Closure FunctionDefinition [Expression]
    | Application Expression Expression

data Type = BooleanType | IntegerType | ClosureType Type Type
type ClosureType = [Type]

typeOf :: Expression -> Type
typeOf (Boolean _) = BooleanType
typeOf (Integer _) = IntegerType
typeOf (Argument t) = t
typeOf (Variable _ t) = t
typeOf (If _ expression _) = typeOf expression
typeOf (Closure function _) = ClosureType argumentType returnType
    where (FunctionDefinition _ argumentType returnType _ _) = function
typeOf (Application closure _) = returnType
    where (ClosureType argumentType returnType) = typeOf closure

sumFunction :: FunctionDefinition
sumFunction = FunctionDefinition "sum" IntegerType (ClosureType IntegerType IntegerType) []
    (Closure sumPartialFunction [Argument IntegerType])

sumPartialFunction :: FunctionDefinition
sumPartialFunction = FunctionDefinition "sum_partial" IntegerType IntegerType [IntegerType]
    (Application (Application (Closure (BuiltInFunction "+") []) (Variable 0 IntegerType)) (Argument IntegerType))

-- This program computes 1 + 1
exampleProgram :: Program
exampleProgram =
    ( [sumFunction, sumPartialFunction]
    , Application (Application (Closure sumFunction []) (Integer 1)) (Integer 2)
    )
