-- This module removes anonymous function from the AST

{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Delambda where

import AST
import Control.Monad.State (State, get, put, evalState)

delambda :: Program -> Program
delambda (assignments, expression) = evalState (delambdaAssignment assignments expression) 0

delambdaAssignment :: [Assignment] -> Expression -> State DelambdaState Program
delambdaAssignment [] expression = delambdaExpression expression
delambdaAssignment (assignment:assignments) expression = do
    let (Assignment symbol value) = assignment

    -- We do not delambda functions that are already assigned to a symbol
    let toExplore = case value of
            (Function _ _ body _) -> body
            other -> other

    (generatedAssignments, generatedResult) <- delambdaExpression toExplore
    (furtherAssignments, furtherResult) <- delambdaAssignment assignments expression
    return (
            generatedAssignments ++
            Assignment symbol generatedResult :
            furtherAssignments
        , furtherResult)

delambdaExpression :: Expression -> State DelambdaState Program
delambdaExpression function@(Function a b body c) = do
    name <- reserveLambda
    (furtherAssignments, furtherBody) <- delambdaExpression body
    return (
            Assignment name (Function a b furtherBody c) :
            furtherAssignments,
            (Variable name)
        )
delambdaExpression (If condition thenBranch elseBranch) = do
    (conditionAssignments, conditionExpression) <- delambdaExpression condition
    (thenAssignments, thenExpression) <- delambdaExpression thenBranch
    (elseAssignment, elseExpression) <- delambdaExpression elseBranch
    return (
            conditionAssignments ++
            thenAssignments ++
            elseAssignment,
            If conditionExpression thenExpression elseExpression
        )
delambdaExpression (Application left right) = do
    (leftAssignments, leftExpression) <- delambdaExpression left
    (rightAssignments, rightExpression) <- delambdaExpression right
    return (
            leftAssignments ++
            rightAssignments,
            Application leftExpression rightExpression
        )
delambdaExpression (Boolean b) = return ([], (Boolean b))
delambdaExpression (Integer n) = return ([], (Integer n))
delambdaExpression (Variable name) = return ([], (Variable name))

type DelambdaState = Int
reserveLambda :: State DelambdaState Symbol
reserveLambda = do
    current <- get
    put (current + 1)
    return $ Symbol ("lambda" ++ show current)
