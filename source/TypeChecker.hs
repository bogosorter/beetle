{-# OPTIONS_GHC -Wincomplete-patterns #-}

module TypeChecker (typeCheck) where

import AST
import Data.Map
import Control.Monad
import Prelude hiding (lookup)


typeCheck :: Expression () -> Either String (Expression Type)
typeCheck program = do
    typeChecked <- typeCheckAux defaultEnvironment program
    case typeOf typeChecked of
        IntegerType -> Right typeChecked
        BooleanType -> Right typeChecked
        _ -> Left "the return of a program must be an integer or a boolean"

type TypedEnvironment = Map String Type

typeCheckAux :: TypedEnvironment -> Expression () -> Either String (Expression Type)

typeCheckAux environment (Boolean value) = Right (Boolean value)
typeCheckAux environment (Integer value) = Right (Integer value)
typeCheckAux environment (Tuple values ()) = do
    checkedValues <- mapM (typeCheckAux environment) values
    let tupleType = Prelude.map typeOf checkedValues
    return $ Tuple checkedValues (TupleType tupleType)
typeCheckAux environment (Struct values ()) = do
    checkedValues <- mapM (typeCheckAux environment) values
    let tupleType = Data.Map.map typeOf checkedValues
    return $ Struct checkedValues (StructType tupleType)
typeCheckAux environment (Variable name ()) = case lookup name environment of
    Nothing -> Left ("symbol " ++ show name ++ " is not defined")
    Just t -> Right (Variable name t)
typeCheckAux environment (StructAccess name base _) = do
    checkedBase <- typeCheckAux environment base
    case checkedBase of
        Struct _ (StructType memberTypes) -> case lookup name memberTypes of
            Just t -> Right (StructAccess name checkedBase t)
            Nothing -> Left ("trying to access non-existing member " ++ name ++ " in a tuple of type " ++ show (typeOf checkedBase))
        _ -> Left ("struct access can only be performed on structs, but tried to access member " ++ name ++ " of type " ++ show checkedBase)

typeCheckAux environment (If condition thenExpression elseExpression ()) = do
    typedCondition <- typeCheckAux environment condition
    let conditionType = typeOf typedCondition
    unless (conditionType == BooleanType) $
        Left $ "condition of an 'if' statement must be a boolean, but the provided expression has type " ++ show conditionType

    typedThen <- typeCheckAux environment thenExpression
    let thenType = typeOf typedThen

    typedElse <- typeCheckAux environment elseExpression
    let elseType = typeOf typedThen

    unless (thenType == elseType) $
        Left $ "both branches of an 'if' statement must have the same type, but types " ++ show thenType ++ " and " ++ show elseType ++ " were provided"

    return (If typedCondition typedThen typedElse thenType)

typeCheckAux environment (Function argumentType returnType argumentName body) = do
    let newEnvironment = insert argumentName argumentType environment
    typedBody <- typeCheckAux newEnvironment body
    let bodyType = typeOf typedBody

    unless (bodyType == returnType) $
        Left $ "return type of function is " ++ show returnType ++ ", but type " ++ show bodyType ++ " was provided"

    return (Function argumentType returnType argumentName typedBody)

typeCheckAux environment (Application function argument ()) = do
    typedFunction <- typeCheckAux environment function
    let functionType = typeOf typedFunction
    (argumentType, returnType) <- case functionType of
        FunctionType argumentType returnType -> Right (argumentType, returnType)
        _ -> Left $ "calling an expression whose type is " ++ show functionType

    typedArgument <- typeCheckAux environment argument
    let providedType = typeOf typedArgument
    unless (providedType == argumentType) $
        Left $ "expected argument of type " ++ show argumentType ++ ", but argument of type " ++ show providedType ++ " was provided"

    return (Application typedFunction typedArgument returnType)

-- When assigning a function, the function itself should be added to the
-- environment to allow recursive calls. This is not the case for variables as
-- they cannot be used in their own definition
typeCheckAux environment (Let name function@(Function argumentType returnType _ _) ensuing ()) = do
    case lookup name environment of
        (Just a) -> Left ("variable " ++ name ++ " was already defined")
        Nothing -> Right()

    let newEnvironment = insert name (FunctionType argumentType returnType) environment
    typedFunction <- typeCheckAux newEnvironment function
    typedEnsuing <- typeCheckAux newEnvironment ensuing
    return (Let name typedFunction typedEnsuing (typeOf typedEnsuing))

typeCheckAux environment (Let name expression ensuing ()) = do
    case lookup name environment of
        (Just a) -> Left ("variable " ++ name ++ " was already defined")
        Nothing -> Right()

    typedExpression <- typeCheckAux environment expression
    let newEnvironment = insert name (typeOf typedExpression) environment
    typedEnsuing <- typeCheckAux newEnvironment ensuing
    return (Let name typedExpression typedEnsuing (typeOf typedEnsuing))

typeCheckAux environment (TupleDestructuring names tuple ensuing ()) = do
    typedTuple <- typeCheckAux environment tuple

    let (TupleType memberTypes) = typeOf typedTuple
    let insertTupleMember (name, t) env = insert name t env
    let newEnvironment = Prelude.foldr insertTupleMember environment (zip names memberTypes)

    typedEnsuing <- typeCheckAux newEnvironment ensuing

    return $ TupleDestructuring names typedTuple typedEnsuing (typeOf typedEnsuing)

defaultEnvironment :: TypedEnvironment
defaultEnvironment = fromList
    [ ("+", FunctionType IntegerType (FunctionType IntegerType IntegerType))
    , ("-", FunctionType IntegerType (FunctionType IntegerType IntegerType))
    -- Note that equality should also operate on other types, but until generic
    -- types are implemented this is not possible
    , ("==", FunctionType IntegerType (FunctionType IntegerType BooleanType))
    , ("<", FunctionType IntegerType (FunctionType IntegerType BooleanType))
    , (">", FunctionType IntegerType (FunctionType IntegerType BooleanType))
    , ("<=", FunctionType IntegerType (FunctionType IntegerType BooleanType))
    , (">=", FunctionType IntegerType (FunctionType IntegerType BooleanType))
    ]
