{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Enclose where

import AST
import TypeChecker
import Closures

import Control.Monad.State
import Data.Map
import Data.List (elemIndex)

enclose :: TypedProgram -> Closures.Program
enclose (assignments, output) = evalState (encloseAux empty assignments output) initialExecutionState

encloseAux :: Environment -> [TypedAssignment] -> TypedExpression -> State ExecutionState Closures.Program

encloseAux env [] output = do
    state <- get
    let build = expressionBuilder state
    let expression = encloseExpression env [] output
    return (Closures.Program (definitions state) (build expression))

encloseAux env (assignment:assignments) output = do
    let (TypedAssignment (Symbol name) assigned) = assignment
    case assigned of

        (TFunction (Symbol argument) argumentType expression returnType) -> do
            let env' = insert name (Closures.Local name (ClosureType (closuredType argumentType) (closuredType returnType))) env
            let env'' = insert argument (Closures.Argument (closuredType argumentType)) env'

            let free = freeVariables env'' [argument] expression
            let special = Prelude.map (\(n, v) -> (n, Closures.typeOf v)) free
            let closureValues = Prelude.map (\(n, v) -> v) free
            let closureTypes = Prelude.map (\(n, v) -> Closures.typeOf v) free

            let body = encloseExpression env'' special expression
            let definition = (FunctionDefinition name (closuredType argumentType) (closuredType returnType) closureTypes body)
            putDefinition definition

            let closure = Closure definition closureValues
            builder <- currentExpressionBuilder
            putExpressionBuilder (\x -> builder (Let name closure x))

            encloseAux env' assignments output

        expression -> do
            let env' = insert name (Closures.Local name (closuredType (TypeChecker.typeOf expression))) env

            let enclosed = encloseExpression env' [] expression
            builder <- currentExpressionBuilder
            putExpressionBuilder (\x -> builder (Let name enclosed x))

            encloseAux env' assignments output



encloseExpression :: Environment -> [(String, Closures.Type)] -> TypedExpression -> Closures.Expression
encloseExpression env free (TBoolean b) = Closures.Boolean b
encloseExpression env free (TInteger n) = Closures.Integer n
encloseExpression env free (TVariable (Symbol name) _)
    | name `elem` ["+", "-"] = (Closure (BuiltInFunction name Closures.IntegerType (ClosureType Closures.IntegerType Closures.IntegerType)) [])
    | name `elem` ["<", ">", "==", "<=", ">="] = (Closure (BuiltInFunction name Closures.IntegerType (ClosureType Closures.IntegerType Closures.BooleanType)) [])
    | otherwise = case getFreeVariable 0 free name of
        Just (index, t) -> Closures.Variable 0 t
        Nothing -> getScope env name
encloseExpression env free (TIf condition thenBranch elseBranch) = Closures.If enclosedCondition enclosedThen enclosedElse
    where enclosedCondition = encloseExpression env free condition
          enclosedThen = encloseExpression env free thenBranch
          enclosedElse = encloseExpression env free elseBranch
encloseExpression env free (TApplication closure argument) = Closures.Application enclosedClosure enclosedArgument
    where enclosedClosure = encloseExpression env free closure
          enclosedArgument = encloseExpression env free argument
encloseExpression env free (TFunction {}) = error "encloseExpression should not be called on a function"

freeVariables :: Environment -> [String] -> TypedExpression -> [(String, Closures.Expression)]
freeVariables env declared (TBoolean _) = []
freeVariables env declared (TInteger _) = []
freeVariables env declared (TVariable (Symbol name) _)
    | name `elem` declared = []
    | name `elem` ["+", "-", "<", ">", "==", "<=", ">="] = []
    | otherwise = [(name, getScope env name)]
freeVariables env declared (TIf condition thenBranch elseBranch) = conditionVariables ++ thenBranchVariables ++ elseBranchVariables
    where conditionVariables = freeVariables env declared condition
          thenBranchVariables = freeVariables env declared thenBranch
          elseBranchVariables = freeVariables env declared elseBranch
freeVariables env declared (TFunction (Symbol argument) _ body _) = freeVariables env declared' body
    where declared' = argument : declared
freeVariables env declared (TApplication closure argument) = closureVariables ++ argumentVariables
    where closureVariables = freeVariables env declared closure
          argumentVariables = freeVariables env declared argument

getFreeVariable :: Int -> [(String, Closures.Type)] -> String -> Maybe (Int, Closures.Type)
getFreeVariable index [] name = Nothing
getFreeVariable index ((n, t):free) name
    | n == name = Just (index, t)
    | otherwise = getFreeVariable (index + 1) free name

data ExecutionState = ExecutionState
    { definitions :: [FunctionDefinition]
    , expressionBuilder :: Closures.Expression -> Closures.Expression
    }

type Environment = Map String Closures.Expression

initialExecutionState :: ExecutionState
initialExecutionState = ExecutionState [] (\x -> x)

getScope :: Environment -> String -> Closures.Expression
getScope env name = case Data.Map.lookup name env of
    Just expression -> expression
    Nothing -> error ("there should be an expression for " ++ name ++ " in the environment")

putDefinition :: FunctionDefinition -> State ExecutionState ()
putDefinition definition = do
    state <- get
    let defs = definitions state
    put state { definitions = definitions state ++ [definition] }

currentExpressionBuilder :: State ExecutionState (Closures.Expression -> Closures.Expression)
currentExpressionBuilder = do
    state <- get
    return (expressionBuilder state)

putExpressionBuilder :: (Closures.Expression -> Closures.Expression) -> State ExecutionState ()
putExpressionBuilder expressionBuilder = do
    state <- get
    put state { expressionBuilder = expressionBuilder }

closuredType :: AST.Type -> Closures.Type
closuredType AST.BooleanType = Closures.BooleanType
closuredType AST.IntegerType = Closures.IntegerType
closuredType (AST.FunctionType _ _) = error "function types should not be directly converted to closured types"
