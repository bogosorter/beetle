{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Closures where

import Debug.Trace
import Data.List (intercalate)
import Distribution.TestSuite (TestInstance(name))

data Program = Program [FunctionDefinition] Expression

-- A function is defined by a name, an argument type, a return type, a closure and an expression
data FunctionDefinition = FunctionDefinition String Type Type ClosureType Expression | BuiltInFunction String Type Type

data Expression
    = Boolean Bool
    | Integer Int
    | Tuple [Expression] Type
    | Argument Type -- The argument of the current function (invalid if we are in the global scope)
    | Variable Int Type -- The index of the variable in the closure
    | Local String Type -- A Variable that was declared with let
    | If Expression Expression Expression
    | Closure FunctionDefinition [Expression]
    | Application Expression Expression
    | Let String Expression Expression
    | TupleDestructuring [String] Expression Expression

data Type = BooleanType | IntegerType | TupleType [Type] | ClosureType Type Type
    deriving Show
type ClosureType = [Type]

typeOf :: Expression -> Type
typeOf (Boolean _) = BooleanType
typeOf (Integer _) = IntegerType
typeOf (Tuple _ t) = t
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
typeOf (TupleDestructuring _ _ expression) = typeOf expression

instance Show Program where
    show (Program definitions expression) = intercalate "\n" (map show definitions) ++ "\n" ++ show expression ++ "\n"

instance Show FunctionDefinition where
    show (FunctionDefinition name argumentType returnType closureType body) =
        name ++ " (" ++ show argumentType ++ " -> " ++ show returnType ++ ") " ++ show closureType ++ "\n" ++ showIndent 4 body ++ "\n"
    show (BuiltInFunction name argumentType returnType) =
        name ++ "(" ++ show argumentType ++ " -> " ++ show returnType ++ ")"

instance Show Expression where
    show expression = showIndent 0 expression

showIndent indent (Boolean b) = (take indent (repeat ' ')) ++ show b
showIndent indent (Integer n) = take indent (repeat ' ') ++ show n
showIndent indent (Tuple members _) = take indent (repeat ' ') ++ "(" ++ intercalate "," (map show members) ++ ")"
showIndent indent (Argument t) = take indent (repeat ' ') ++ "argument"
showIndent indent (Variable index _) = take indent (repeat ' ') ++ "variable " ++ show index
showIndent indent (Local name _) = take indent (repeat ' ') ++ name
showIndent indent (If condition thenBranch elseBranch) =
    take indent (repeat ' ') ++ "if\n" ++
        showIndent (indent + 4) condition ++ "\n" ++
    take indent (repeat ' ') ++ "then\n" ++
        showIndent (indent + 4) thenBranch ++ "\n" ++
    take indent (repeat ' ') ++ "else\n" ++
        showIndent (indent + 4) elseBranch ++ "\n"
showIndent indent (Closure function expressions) =
    take indent (repeat ' ') ++ "closure of definition " ++ name ++ "\n" ++
        intercalate "\n" (map (showIndent (indent + 4)) expressions)
    where name = case function of
            (FunctionDefinition name _ _ _ _) -> name
            (BuiltInFunction name _ _) -> name
showIndent indent (Application closure argument) =
    take indent (repeat ' ') ++ "application of\n" ++
        showIndent (indent + 4) argument ++ "\n" ++
    take indent (repeat ' ') ++ "to\n" ++
        showIndent (indent + 4) closure
showIndent indent (Let name defined expression) =
    take indent (repeat ' ') ++ "let " ++ name ++ " =\n" ++
        showIndent (indent + 4) defined ++ "\n" ++
    take indent (repeat ' ') ++ "in\n" ++
        showIndent (indent + 4) expression
showIndent indent (TupleDestructuring names value expression) =
    take indent (repeat ' ') ++ "let " ++ "(" ++ intercalate "," (map show names) ++ ")" ++ " =\n" ++
        showIndent (indent + 4) value ++ "\n" ++
    take indent (repeat ' ') ++ "in\n" ++
        showIndent (indent + 4) expression
