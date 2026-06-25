{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Encloser (encloseProgram) where

import AST
import Closures (Program(..), FunctionDefinition(..))
import qualified Closures (Type(..), Expression(..))

import Data.Set (Set, singleton, empty, union, unions, delete)
import Data.Map (elems)
import Control.Monad.State


encloseProgram :: TypedExpression -> Closures.Program
encloseProgram program = Program definitions expression
    where (expression, ClosureState {definitions = definitions}) =
            runState (enclose emptyEnvironment program) initialState


enclose :: Environment -> TypedExpression -> State ClosureState Closures.Expression
enclose = error "not implemented"

freeVariables :: TypedExpression -> Set String
freeVariables expression = case expression of

    Boolean {} -> empty
    Integer {} -> empty
    Variable { variableName = name } -> singleton name

    Tuple { tupleMembers = members } ->
        unions $ map freeVariables members

    Record { recordMembers = members } ->
        unions $ map freeVariables (elems members)

    Function { argumentName = argument, body = body } ->
        delete argument $ freeVariables body

    If { condition = condition, left = left, right = right} ->
        freeVariables condition `union` freeVariables left `union` freeVariables right

    Application { function = function, argument = argument} ->
        freeVariables function `union` freeVariables argument

    RecordMember { record = record } -> freeVariables record
    TypeDeclaration { body = body } -> freeVariables body

    Assignment { variableValue = value, body = body} ->
        freeVariables value `union` freeVariables body

    TupleDestructuring { tuple = tuple, body = body} ->
        freeVariables tuple `union` freeVariables body



-- State and environment utils

data ClosureState = ClosureState
    { definitions :: [FunctionDefinition]
    }

initialState :: ClosureState
initialState = ClosureState []

data Environment = Environment
    {
    }

emptyEnvironment :: Environment
emptyEnvironment = Environment
