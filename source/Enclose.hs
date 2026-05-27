{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Enclose where

import AST
import TypeChecker
import Closures

import Control.Monad.State
import Data.Map
import Data.List (elemIndex)
import System.Console.Terminfo (functionKey)
import Debug.Trace

enclose :: AST.Expression AST.Type -> Program
enclose expression = Program closedDefinitions closedExpression
    where (closedExpression, state) = runState (encloseExpression empty [] expression) initialExecutionState
          closedDefinitions = definitions state

encloseExpression :: Environment -> [(String, Closures.Expression)] -> AST.Expression AST.Type -> State ExecutionState Closures.Expression

encloseExpression env free (AST.Let name value ensuing _) = do
    let env' = insert name (Closures.Local name (closuredType (AST.typeOf value))) env

    enclosed <- encloseExpression env' free value
    closedEnsuing <- encloseExpression env' free ensuing
    return $ Closures.Let name enclosed closedEnsuing

encloseExpression env free (AST.Function argumentType returnType argument body) = do
    name <- reserveLambda

    let env' = insert name (Closures.Local name (ClosureType (closuredType argumentType) (closuredType returnType))) env
    let env'' = insert argument (Closures.Argument (closuredType argumentType)) env'

    let tmp = Prelude.map (\(n, v) -> (n, Closures.typeOf v)) free
    let free = freeVariables env'' [argument] tmp body
    trace ("free variables in function " ++ name ++ ": " ++ show free)(do
        let special = Prelude.map (\(n, v) -> (n, Closures.typeOf v)) free
        let closureValues = Prelude.map (\(n, v) -> v) free
        let closureTypes = Prelude.map (\(n, v) -> Closures.typeOf v) free

        trace ("calling encloseExpression with free values " ++ show free)(do
            enclosed <- encloseExpression env'' free body
            let definition = (FunctionDefinition name (closuredType argumentType) (closuredType returnType) closureTypes enclosed)
            putDefinition definition

            return $ Closure definition closureValues))

encloseExpression env free (AST.Boolean b) = return $ Closures.Boolean b
encloseExpression env free (AST.Integer n) = return $ Closures.Integer n
encloseExpression env free (AST.Variable name _)
    | name `elem` ["+", "-"] = return (Closure (BuiltInFunction name Closures.IntegerType (ClosureType Closures.IntegerType Closures.IntegerType)) [])
    | name `elem` ["<", ">", "==", "<=", ">="] = return (Closure (BuiltInFunction name Closures.IntegerType (ClosureType Closures.IntegerType Closures.BooleanType)) [])
    | otherwise = case getFreeVariable 0 freeTypes name of
        Just (index, t) -> trace ("got free variable " ++ name) (return $ Closures.Variable index t)
        Nothing -> trace ("didn't get free variable " ++ name ++ " " ++ show env ++ " " ++ show free)(return $ getScope env name)
    where freeTypes = Prelude.map (\(s, e) -> (s, Closures.typeOf e)) free
encloseExpression env free (AST.If condition thenBranch elseBranch _) = do
    enclosedCondition <- encloseExpression env free condition
    enclosedThen <- encloseExpression env free thenBranch
    enclosedElse <- encloseExpression env free elseBranch
    return $ Closures.If enclosedCondition enclosedThen enclosedElse
encloseExpression env free (AST.Application closure argument _) = do
    enclosedClosure <- encloseExpression env free closure
    enclosedArgument <- encloseExpression env free argument
    return $ Closures.Application enclosedClosure enclosedArgument

freeVariables :: Environment -> [String] -> [(String, Closures.Type)] -> AST.Expression AST.Type -> [(String, Closures.Expression)]
freeVariables env declared currentFree (AST.Boolean _) = []
freeVariables env declared currentFree (AST.Integer _) = []
freeVariables env declared currentFree (AST.Variable name _)
    | name `elem` declared = []
    | name `elem` ["+", "-", "<", ">", "==", "<=", ">="] = []
    | otherwise = case getFreeVariable 0 currentFree name of
        Just (index, t) -> trace ("got free variable " ++ name) [(name, Closures.Variable index t)]
        Nothing -> trace ("didn't get free variable " ++ name ++ " ") [(name, getScope env name)]
freeVariables env declared currentFree (AST.If condition thenBranch elseBranch _) = conditionVariables ++ thenBranchVariables ++ elseBranchVariables
    where conditionVariables = freeVariables env declared currentFree condition
          thenBranchVariables = freeVariables env declared currentFree thenBranch
          elseBranchVariables = freeVariables env declared currentFree elseBranch
freeVariables env declared currentFree (AST.Function _ _ argument body) = freeVariables env declared' currentFree body
    where declared' = argument : declared
freeVariables env declared currentFree (AST.Application closure argument _) = closureVariables ++ argumentVariables
    where closureVariables = freeVariables env declared currentFree closure
          argumentVariables = freeVariables env declared currentFree argument
freeVariables env declared currentFree (AST.Let name value ensuing _) = valueVariables ++ ensuingVariables
    where newDeclared = name : declared
          valueVariables = freeVariables env declared currentFree value
          ensuingVariables = freeVariables env newDeclared currentFree ensuing


getFreeVariable :: Int -> [(String, Closures.Type)] -> String -> Maybe (Int, Closures.Type)
getFreeVariable index [] name = Nothing
getFreeVariable index ((n, t):free) name
    | n == name = Just (index, t)
    | otherwise = getFreeVariable (index + 1) free name

data ExecutionState = ExecutionState
    { definitions :: [FunctionDefinition]
    , lambdaNumber :: Int
    }

type Environment = Map String Closures.Expression

initialExecutionState :: ExecutionState
initialExecutionState = ExecutionState [] 0

getScope :: Environment -> String -> Closures.Expression
getScope env name = case Data.Map.lookup name env of
    Just expression -> expression
    Nothing -> error ("there should be an expression for " ++ name ++ " in the environment")

putDefinition :: FunctionDefinition -> State ExecutionState ()
putDefinition definition = do
    state <- get
    let defs = definitions state
    put state { definitions = definitions state ++ [definition] }

reserveLambda :: State ExecutionState String
reserveLambda = do
    state <- get
    let current = lambdaNumber state
    put state { lambdaNumber = current + 1 }
    return $ "lambda" ++ show current

closuredType :: AST.Type -> Closures.Type
closuredType AST.BooleanType = Closures.BooleanType
closuredType AST.IntegerType = Closures.IntegerType
closuredType (AST.FunctionType a b) = Closures.ClosureType (closuredType a) (closuredType b)
