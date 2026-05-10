{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Enclose where

import AST
import TypeChecker
import Closures

import Control.Monad.State
import Data.Map
import Data.List (elemIndex)
import System.Console.Terminfo (functionKey)

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

-- If we get here, this must be a lambda function
encloseExpression env free (AST.Function argumentType returnType argument body) = do
    name <- reserveLambda

    let env' = insert name (Closures.Local name (ClosureType (closuredType argumentType) (closuredType returnType))) env
    let env'' = insert argument (Closures.Argument (closuredType argumentType)) env'

    let free = freeVariables env'' [argument] body
    let special = Prelude.map (\(n, v) -> (n, Closures.typeOf v)) free
    let closureValues = Prelude.map (\(n, v) -> v) free
    let closureTypes = Prelude.map (\(n, v) -> Closures.typeOf v) free

    enclosed <- encloseExpression env'' free body
    let definition = (FunctionDefinition name (closuredType argumentType) (closuredType returnType) closureTypes enclosed)
    putDefinition definition

    return $ Closure definition closureValues

encloseExpression env free (AST.Boolean b) = return $ Closures.Boolean b
encloseExpression env free (AST.Integer n) = return $ Closures.Integer n
encloseExpression env free (AST.Variable name _)
    | name `elem` ["+", "-"] = return (Closure (BuiltInFunction name Closures.IntegerType (ClosureType Closures.IntegerType Closures.IntegerType)) [])
    | name `elem` ["<", ">", "==", "<=", ">="] = return (Closure (BuiltInFunction name Closures.IntegerType (ClosureType Closures.IntegerType Closures.BooleanType)) [])
    | otherwise = case getFreeVariable 0 freeTypes name of
        Just (index, t) -> return $ Closures.Variable 0 t
        Nothing -> return $ getScope env name
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

freeVariables :: Environment -> [String] -> AST.Expression AST.Type -> [(String, Closures.Expression)]
freeVariables env declared (AST.Boolean _) = []
freeVariables env declared (AST.Integer _) = []
freeVariables env declared (AST.Variable name _)
    | name `elem` declared = []
    | name `elem` ["+", "-", "<", ">", "==", "<=", ">="] = []
    | otherwise = [(name, getScope env name)]
freeVariables env declared (AST.If condition thenBranch elseBranch _) = conditionVariables ++ thenBranchVariables ++ elseBranchVariables
    where conditionVariables = freeVariables env declared condition
          thenBranchVariables = freeVariables env declared thenBranch
          elseBranchVariables = freeVariables env declared elseBranch
freeVariables env declared (AST.Function _ _ argument body) = freeVariables env declared' body
    where declared' = argument : declared
freeVariables env declared (AST.Application closure argument _) = closureVariables ++ argumentVariables
    where closureVariables = freeVariables env declared closure
          argumentVariables = freeVariables env declared argument
freeVariables env declared (AST.Let name value ensuing _) = ensuingVariables
    where newDeclared = name : declared
          ensuingVariables = freeVariables env newDeclared ensuing


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
