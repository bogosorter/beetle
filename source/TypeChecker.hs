module TypeChecker (typeCheckProgram, TypeError(..)) where

import AST
import Text.Megaparsec (SourcePos)
import qualified Data.Map as Map
import Control.Monad (unless, when)


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
        argumentType <- desugar position env argumentType
        returnType <- desugar position env returnType

        let env' = insertVariableType argumentName argumentType env
        typedBody <- typeCheck env' body

        unless (getType typedBody == returnType) $
            Left $ TypeError position "function return type does not match body type"

        Right $ Function argumentType returnType argumentName typedBody (FunctionType argumentType returnType)

    If {} -> do
        typedCondition <- typeCheck env (condition expression)
        unless (getType typedCondition == BooleanType) $
            Left $ TypeError position "the condition of an if must always be a boolean expression"

        typedLeft <- typeCheck env (left expression)
        typedRight <- typeCheck env (right expression)

        unless (getType typedLeft == getType typedLeft) $
            Left $ TypeError position "the branches of an if expression must have the same return type"

        Right $ If typedCondition typedLeft typedRight (getType typedLeft)

    Application {} -> do
        typedFunction <- typeCheck env (function expression)
        (argumentType, returnType) <- case getType typedFunction of
            FunctionType argumentType returnType -> Right $ (argumentType, returnType)
            t -> Left $ TypeError position ("can only apply functions, but got type " ++ show t)

        typedArgument <- typeCheck env (argument expression)
        -- We do not overwrite the initial variable to keep user-defined
        -- variables in the error output
        argumentType' <- desugar position env argumentType
        returnType <- desugar position env returnType
        unless (getType typedArgument == argumentType') $
            Left $ TypeError (getPosition $ argument expression) ("expected an argument of type " ++ show argumentType ++ ", but got argument of type " ++ show (getType typedArgument))

        Right $ Application typedFunction typedArgument returnType

    Variable {} -> do
        let name = variableName expression
        let types = variableTypes env
        case Map.lookup name types of
            Just t -> do
                t <- desugar position env t
                Right $ Variable name t
            _ -> Left $ TypeError position ("couldn't find name " ++ name)

    RecordMember {} -> do
        typedRecord <- typeCheck env (record expression)
        memberTypes <- case getType typedRecord of
            RecordType memberTypes -> Right $ memberTypes
            t -> Left $ TypeError position ("can only access members of records, but got type " ++ show t)

        let name = memberName expression
        case Map.lookup name memberTypes of
            Just t -> Right $ RecordMember typedRecord name t
            _ -> Left $ TypeError position ("trying to access inexistent member " ++ name ++ " of this record")

    -- Type shadowing is not allowed
    -- The type declaration is removed during type checking
    TypeDeclaration {} -> do
        let name = typeName expression
        unless (isAvailable env name) $
            Left $ TypeError position ("type " ++ name ++ " has already been declared")

        let env' = insertUserType (typeName expression) (aliasedType expression) env
        typeCheck env' (body expression)

    Assignment {} -> do
        let name = variableName expression
        when (name == "_") $
            Left $ TypeError position ("assigning to a hole")

        -- For the moment, arbitrary recursion is not allowed. For instance, a
        -- lambda cannot refer to the variable that holds it. This can only
        -- happen if there is an assignment whose direct child is a function
        -- (whose type is know a priori). As such, if this is such a function
        -- its type is inserted before evaluation, if not it is inserted after
        -- evaluation
        (typedValue, env') <- case (variableValue expression) of

            Function { argumentType = at, returnType = rt } -> do
                let env' = insertVariableType name (FunctionType at rt) env
                typedValue <- typeCheck env' (variableValue expression)
                return (typedValue, env')

            _ -> do
                typedValue <- typeCheck env (variableValue expression)
                let env' = insertVariableType name (getType typedValue) env
                return (typedValue, env')


        typedBody <- typeCheck env' (body expression)
        Right $ Assignment name typedValue typedBody (getType typedBody)

    TupleDestructuring {} -> do
        let names = destructuredNames expression

        typedTuple <- typeCheck env (tuple expression)
        memberTypes <- case getType typedTuple of
            TupleType memberTypes -> Right $ memberTypes
            t -> Left $ TypeError position ("attempting to destructure tuple, but got type " ++ show t)

        let env' = foldr (uncurry insertVariableType) env (zip names memberTypes)
        typedBody <- typeCheck env' (body expression)

        Right $ TupleDestructuring names typedTuple typedBody (getType typedBody)

    where position = getPosition expression

desugar :: SourcePos -> Environment -> Type -> Either TypeError Type
desugar position env (UserType name) = do
    let types = userTypes env
    case Map.lookup name types of
        Just t -> desugar position env t
        Nothing -> Left $ TypeError position ("couldn't find type " ++ name)
desugar position env (TupleType memberTypes) = do
    desugaredMemberTypes <- mapM (desugar position env) memberTypes
    Right $ TupleType desugaredMemberTypes
desugar position env (RecordType memberTypes) = do
    desugaredMemberTypes <- mapM (desugar position env) memberTypes
    Right $ RecordType desugaredMemberTypes
desugar position env (FunctionType argumentType returnType) = do
    desugaredArgumentType <- desugar position env argumentType
    desugaredReturnType <- desugar position env returnType
    Right $ FunctionType desugaredArgumentType desugaredReturnType
desugar _ _ t = Right $ t


-- Environment utils

data Environment = Environment
    { variableTypes :: Map.Map String Type
    , userTypes :: Map.Map String Type
    }

emptyEnvironment :: Environment
emptyEnvironment = Environment defaultFunctions Map.empty

defaultFunctions :: Map.Map String Type
defaultFunctions = Map.fromList
    [ ("*", FunctionType IntegerType (FunctionType IntegerType IntegerType))
    , ("/", FunctionType IntegerType (FunctionType IntegerType IntegerType))
    , ("mod", FunctionType IntegerType (FunctionType IntegerType IntegerType))
    , ("rem", FunctionType IntegerType (FunctionType IntegerType IntegerType))
    , ("+", FunctionType IntegerType (FunctionType IntegerType IntegerType))
    , ("-", FunctionType IntegerType (FunctionType IntegerType IntegerType))
    , ("==", FunctionType IntegerType (FunctionType IntegerType BooleanType))
    , (">=", FunctionType IntegerType (FunctionType IntegerType BooleanType))
    , ("<=", FunctionType IntegerType (FunctionType IntegerType BooleanType))
    , (">", FunctionType IntegerType (FunctionType IntegerType BooleanType))
    , ("<", FunctionType IntegerType (FunctionType IntegerType BooleanType))
    , ("not", FunctionType BooleanType BooleanType)
    , ("and", FunctionType BooleanType (FunctionType BooleanType BooleanType))
    , ("or", FunctionType BooleanType (FunctionType BooleanType BooleanType))
    ]

isAvailable :: Environment -> String -> Bool
isAvailable env name = not (Map.member name variables) && not (Map.member name types)
    where variables = variableTypes env
          types = userTypes env

insertVariableType :: String -> Type -> Environment  -> Environment
insertVariableType name t env = Environment newTypes (userTypes env)
    where newTypes = Map.insert name t (variableTypes env)

insertUserType :: String -> Type -> Environment -> Environment
insertUserType name t env = Environment (variableTypes env) newTypes
    where newTypes = Map.insert name t (userTypes env)


instance Show TypeError where
    show (TypeError _ e) = e
