{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Types (TypedProgram, typeCheck, TypedAssignment(..), TypedExpression(..)) where

import AST
import Data.Map
import Control.Monad
import Prelude hiding (lookup)

type TypedProgram = ([TypedAssignment], TypedExpression)
type TypedEnvironment = Map Symbol Type

data TypedAssignment = TypedAssignment Symbol TypedExpression
data TypedExpression
    = TBoolean Bool
    | TInteger Int
    | TVariable Symbol Type
    | TIf TypedExpression TypedExpression TypedExpression
    | TFunction Symbol Type TypedExpression
    | TApplication TypedExpression TypedExpression
    deriving Show


typeCheck :: Program -> Either String TypedProgram
typeCheck = typeCheckAux defaultEnvironment

typeCheckAux :: TypedEnvironment -> Program -> Either String TypedProgram
typeCheckAux environment (assignments, expression) = do
    (assignmentTypes, newEnvironment) <- typeCheckAssignments environment assignments
    output <- typeCheckExpression newEnvironment expression
    return (assignmentTypes, output)

typeCheckAssignments :: TypedEnvironment -> [Assignment] -> Either String ([TypedAssignment], TypedEnvironment)
typeCheckAssignments environment [] = Right ([], environment)
typeCheckAssignments environment (x:xs) = do
    result <- typeCheckAssignment environment x
    let (TypedAssignment name expression) = result
    let expressionType = typeOf expression
    let newEnvironment = insert name expressionType environment
    (remaining, finalEnvironment) <- typeCheckAssignments newEnvironment xs
    return (result : remaining, finalEnvironment)

typeCheckAssignment :: TypedEnvironment -> Assignment -> Either String TypedAssignment
typeCheckAssignment environment (Assignment symbol expression) = do
    result <- typeCheckExpression environment expression
    return (TypedAssignment symbol result)

typeCheckExpression :: TypedEnvironment -> Expression -> Either String TypedExpression
typeCheckExpression environment (Boolean value) = Right (TBoolean value)
typeCheckExpression environment (Integer value) = Right (TInteger value)
typeCheckExpression environment (Variable name) = case lookup name environment of
    Nothing -> Left ("symbol " ++ show name ++ " is not defined")
    Just t -> Right (TVariable name t)

typeCheckExpression environment (If condition thenExpression elseExpression) = do
    typedCondition <- typeCheckExpression environment condition
    let conditionType = typeOf typedCondition
    unless (conditionType == BooleanType) $
        Left $ "condition of an 'if' statement must be a boolean, but the provided expression has type " ++ show conditionType

    typedThen <- typeCheckExpression environment thenExpression
    let thenType = typeOf typedThen

    typedElse <- typeCheckExpression environment elseExpression
    let elseType = typeOf typedThen

    unless (thenType == elseType) $
        Left $ "both branches of an 'if' statement must have the same type, but types " ++ show thenType ++ " and " ++ show elseType ++ " were provided"

    return (TIf typedCondition typedThen typedElse)

typeCheckExpression environment (Function argumentName argumentType body) = do
    let newEnvironment = insert argumentName argumentType environment
    typedBody <- typeCheckExpression newEnvironment body

    return (TFunction argumentName argumentType typedBody)

typeCheckExpression environment (Application function argument) = do
    typedFunction <- typeCheckExpression environment function
    let functionType = typeOf typedFunction
    (argumentType, returnType) <- case functionType of
        FunctionType argumentType returnType -> Right (argumentType, returnType)
        _ -> Left $ "calling an expression whose type is " ++ show functionType

    typedArgument <- typeCheckExpression environment argument
    let providedType = typeOf typedArgument
    unless (providedType == argumentType) $
        Left $ "expected argument of type " ++ show argumentType ++ ", but argument of type " ++ show providedType ++ " was provided"

    return (TApplication typedFunction typedArgument)

typeOf :: TypedExpression -> Type
typeOf (TBoolean _) = BooleanType
typeOf (TInteger _) = IntegerType
typeOf (TVariable _ t) = t
typeOf (TIf _ thenBranch _) = typeOf thenBranch
typeOf (TFunction _ argumentType body) = FunctionType argumentType (typeOf body)
typeOf (TApplication expression _) = case typeOf expression of
    FunctionType _ returnType -> returnType
    _ -> error ("typeOf called on application to a non-function: " ++ show (typeOf expression))

defaultEnvironment :: TypedEnvironment
defaultEnvironment = fromList
    [ (Symbol "+", FunctionType IntegerType (FunctionType IntegerType IntegerType))
    , (Symbol "-", FunctionType IntegerType (FunctionType IntegerType IntegerType))
    -- Note that equality should also operate on other types, but until generic
    -- types are implemented this is not possible
    , (Symbol "==", FunctionType IntegerType (FunctionType IntegerType BooleanType))
    ]
