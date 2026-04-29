module Closures where

import qualified AST
import qualified TypeChecker

type Program = ([FunctionDefinition], [Assignment], Expression)

-- A function is defined by a name, an argument type, a return type, a closure and an expression
data FunctionDefinition = FunctionDefinition String Type Type Closure Expression | BuiltInFunction String
data Assignment = Assignment String Expression
data Expression
    = Boolean Bool
    | Integer Int
    | Argument -- The argument of the current function (invalid if we are in the global scope)
    | Variable Int -- The index of the variable in the closure
    | If Expression Expression Expression Type
    | Function FunctionDefinition
    | Application Expression Expression

data Type = BooleanType | IntegerType
type Closure = [Type]
data ClosureAssignment

sumFunction :: FunctionDefinition
sumFunction = FunctionDefinition "sum" IntegerType IntegerType [IntegerType]
    (Application (Application (Function (BuiltInFunction "+")) (Variable 0)) (Argument))

exampleProgram :: Program
exampleProgram =
    ( [sumFunction]
    , [Assignment "x" (Integer 1)]
    , Application (Function sumFunction) (Integer 1)
    )
