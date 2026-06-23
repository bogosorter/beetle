{-# OPTIONS_GHC -Wno-name-shadowing #-}
module TypeChecker (typeCheckProgram) where

import AST
import Text.Megaparsec (SourcePos)
import qualified Data.Map as Map
import Control.Monad (unless)


data TypeError = TypeError SourcePos String

typeCheckProgram :: SourceExpression -> Either TypeError TypedExpression
typeCheckProgram program = do
    checked <- typeCheck emptyEnvironment program
    case getType checked of
        IntegerType -> Right checked
        BooleanType -> Right checked
        _ -> Left $ TypeError (getPosition program) "every program must return an integer or a boolean"


typeCheck :: Environment -> SourceExpression -> Either TypeError TypedExpression
typeCheck env expression = case expression of
    Integer {} -> Right $ Integer (integerValue expression) IntegerType
    Boolean {} -> Right $ Boolean (booleanValue expression) BooleanType

    Tuple {} -> do
        typedMembers <- mapM (typeCheck env) (tupleMembers expression)
        let tupleType = TupleType $ map getType typedMembers
        Right $ Tuple typedMembers tupleType

    Record {} -> do
        typedMembers <- mapM (typeCheck env) (recordMembers expression)
        let tupleType = RecordType $ Map.map getType typedMembers
        Right $ Record typedMembers tupleType

    Function {} -> do
        let Function argumentType returnType argumentName body _ = expression
        let env' = insertVariableType env argumentName argumentType
        typedBody <- typeCheck env' body

        unless (getType typedBody == returnType) $
            Left $ TypeError (getPosition expression) "function return type does not match body type"

        Right $ Function argumentType returnType argumentName typedBody (FunctionType argumentType returnType)

    If {} -> do
        typedCondition <- typeCheck env (condition expression)
        unless (getType typedCondition == BooleanType) $
            Left $ TypeError (getPosition expression) "the condition of an if must always be a boolean expression"

        typedLeft <- typeCheck env (left expression)
        typedRight <- typeCheck env (right expression)
        unless (getType typedLeft == getType typedRight) $
            Left $ TypeError (getPosition expression) "the branches of an if expression must have the same return type"

        Right $ If typedCondition typedLeft typedRight (getType typedLeft)

    Application {} -> do
        typedFunction <- typeCheck env (function expression)
        (argumentType, returnType) <- case getType typedFunction of
            FunctionType argumentType returnType -> Right $ (argumentType, returnType)
            t -> Left $ TypeError (getPosition expression) ("can only apply functions, but got type " ++ show t)


        typedArgument <- typeCheck env (argument expression)
        unless (getType typedArgument == argumentType) $
            Left $ TypeError (getPosition expression) ("expected an argument of type " ++ show argumentType ++ ", but got argument of type " ++ show (getType typedArgument))

        Right $ Application typedFunction typedArgument returnType

    Variable {} -> do
        let name = variableName expression
        let types = variableTypes env
        case Map.lookup name types of
            Just t -> Right $ Variable name t
            _ -> Left $ TypeError (getPosition expression) ("couldn't find name " ++ name)

    RecordMember {} -> do
        typedRecord <- typeCheck env (record expression)
        memberTypes <- case getType typedRecord of
            RecordType memberTypes -> Right $ memberTypes
            t -> Left $ TypeError (getPosition expression) ("can only access members of records, but got type " ++ show t)

        let name = memberName expression
        case Map.lookup name memberTypes of
            Just t -> Right $ RecordMember typedRecord name t
            _ -> Left $ TypeError (getPosition expression) ("trying to access inexistent member " ++ name ++ " of this record")

    -- Type shadowing is not allowed
    -- The type declaration is removed during type checking
    TypeDeclaration {} -> do
        let name = typeName expression
        unless (isAvailable env name) $
            Left $ TypeError (getPosition expression) ("type " ++ name ++ " has already been declared")

        let env' = insertUserType env (typeName expression) (aliasedType expression)
        typeCheck env' (body expression)

    -- Variable shadowing is not allowed
    Assignment {} -> do
        let name = variableName expression
        checkAvailableVariable expression env name

        typedValue <- typeCheck env (variableValue expression)
        let env' = insertVariableType env name (getType typedValue)
        typedBody <- typeCheck env' (body expression)

        Right $ Assignment name typedValue typedBody (getType typedBody)

    TupleDestructuring {} -> do
        let names = destructuredNames expression
        mapM_ (checkAvailableVariable expression env) names

        typedTuple <- typeCheck env (tuple expression)
        memberTypes <- case getType typedTuple of
            TupleType memberTypes -> Right $ memberTypes
            t -> Left $ TypeError (getPosition expression) ("attempting to destructure tuple, but got type " ++ show t)

        let env' = foldr (\(n, t) -> \env -> insertVariableType env n t) env (zip names memberTypes)
        typedBody <- typeCheck env' (body expression)

        Right $ TupleDestructuring names typedTuple typedBody (getType typedBody)


data Environment = Environment
    { variableTypes :: Map.Map String Type
    , userTypes :: Map.Map String Type
    }

emptyEnvironment :: Environment
emptyEnvironment = Environment defaultFunctions Map.empty

defaultFunctions :: Map.Map String Type
defaultFunctions = Map.fromList
    [ ("+", FunctionType IntegerType (FunctionType IntegerType IntegerType))
    , ("-", FunctionType IntegerType (FunctionType IntegerType IntegerType))
    , ("==", FunctionType IntegerType (FunctionType IntegerType BooleanType))
    , (">=", FunctionType IntegerType (FunctionType IntegerType BooleanType))
    , ("<=", FunctionType IntegerType (FunctionType IntegerType BooleanType))
    , (">", FunctionType IntegerType (FunctionType IntegerType BooleanType))
    , ("<", FunctionType IntegerType (FunctionType IntegerType BooleanType))
    ]

isAvailable :: Environment -> String -> Bool
isAvailable env name = not (Map.member name variables) && not (Map.member name types)
    where variables = variableTypes env
          types = userTypes env

insertVariableType :: Environment -> String -> Type -> Environment
insertVariableType env name t = Environment newTypes (userTypes env)
    where newTypes = Map.insert name t (variableTypes env)

insertUserType :: Environment -> String -> Type -> Environment
insertUserType env name t = Environment (variableTypes env) newTypes
    where newTypes = Map.insert name t (userTypes env)

checkAvailableVariable :: SourceExpression -> Environment -> String -> Either TypeError ()
checkAvailableVariable expression env name = case isAvailable env name of
    True -> Right ()
    False -> Left $ TypeError (getPosition expression) ("variable " ++ name ++ " has already been declared")


instance Show TypeError where
    show (TypeError _ e) = e
