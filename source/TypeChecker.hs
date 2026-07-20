module TypeChecker (typeCheckProgram, TypeError(..)) where

import AST
import Text.Megaparsec (SourcePos)
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.Set as Set
import Data.Maybe (isJust)
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

    Constructor {} -> do
        let name = constructor expression
        case Map.lookup name (constructors env) of
            Just sumType -> do
                let constructors = case Map.lookup sumType (sumTypes env) of
                        Just constructors -> constructors
                        _ -> error ("got constructor to something that is not a user type: " ++ show sumType)
                typedValue <- typeCheck env (value expression)

                let expectedValueType = constructors ! name
                    valueType = getType typedValue

                desugaredExpectedType <- desugar position env expectedValueType
                when (desugaredExpectedType /= valueType) $
                    Left $ TypeError (getPosition $ value expression) ("expected type " ++ show expectedValueType ++ ", but got value of type " ++ show valueType)

                return $ Constructor name typedValue (UserType sumType)

            Nothing -> Left $ TypeError position ("couldn't find constructor " ++ name)

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

    Case {} -> do
        typedScrutinee <- typeCheck env (scrutinee expression)
        constructors <- case getType typedScrutinee of
            UserType userType -> case Map.lookup userType (sumTypes env) of
                Just constructors -> return constructors
                _ -> Left $ TypeError (getPosition $ scrutinee expression) ("the scrutinee of a case expression must be a sum type, but got type " ++ userType)
            t -> Left $ TypeError (getPosition $ scrutinee expression) ("the scrutinee of a case expression must be a sum type, but got type " ++ show t)

        -- The three conditions asserted below are also enough to prove that
        -- every possible case is covered:
        --     - The constructors in the case branches cannot have duplicates
        --     - The number of branches must be equal to the number of
        --       constructors of the sum type
        --     - Every constructor in the branches must belong to the sum type
        --       of the scrutinee

        let branchConstructors = [constructor | (constructor, _, _) <- branches expression]
        when (hasDuplicates branchConstructors) $
            Left $ TypeError position "case branches have duplicate constructors"
        unless (length branchConstructors == Map.size constructors || isJust (defaultBranch expression)) $
            Left $ TypeError position "cannot have less branches in a case expression than constructors in the sum type"

        let typeCheckBranch :: (String, String, SourceExpression) -> Either TypeError (String, String, TypedExpression)
            typeCheckBranch (constructor, introducedVariable, value) = do
                unless (Map.member constructor constructors) $
                    Left $ TypeError position ("constructor " ++ show constructor ++ " does not exist in type " ++ show (getType typedScrutinee))

                let introducedType = constructors ! constructor
                    env' = insertVariableType introducedVariable introducedType env

                typedValue <- typeCheck env' value
                return (constructor, introducedVariable, typedValue)

        typedBranches <- mapM typeCheckBranch (branches expression)
        typedDefault <- case (defaultBranch expression) of
            Just branch -> do
                typed <- typeCheck env branch
                return $ Just typed
            Nothing -> return Nothing

        let branchValueTypes = [getType value | (_, _, value) <- typedBranches]
            branchValueTypesWithDefault = case typedDefault of
                Just branch -> branchValueTypes ++ [getType branch]
                Nothing -> branchValueTypes
        unless (allEqual branchValueTypesWithDefault) $
            Left $ TypeError position "all the return types of a case expression's branches must be equal"

        return $ Case typedScrutinee typedBranches typedDefault (branchValueTypesWithDefault !! 0)

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
    TypeAssignment {} -> do
        let name = assignedName expression
        unless (isAvailable env name) $
            Left $ TypeError position ("type " ++ name ++ " has already been declared")

        let t = assignedType expression
            env' = case t of
                SumType constructors ->
                    let env' = insertSumType name constructors env
                        constructorTypes = [(constructor, name) | constructor <- Map.keys constructors]
                    in Prelude.foldr (uncurry insertConstructor) env' constructorTypes
                _ -> insertTypeAlias name t env

        typedBody <- typeCheck env' (body expression)

        -- Only sum type assignments are left on the AST
        case t of
            SumType _ -> return $ TypeAssignment name t typedBody (getType typedBody)
            _ -> return typedBody

    Assignment {} -> do
        let name = assignedName expression
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
    let aliases = typeAliases env
    case Map.lookup name aliases of
        Just t -> desugar position env t
        Nothing -> do
            let sums = sumTypes env
            case Map.lookup name sums of
                Just _ -> Right $ UserType name
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
    , typeAliases :: Map.Map String Type
    , sumTypes :: Map.Map String (Map.Map String Type)
    , constructors :: Map.Map String String
    }

emptyEnvironment :: Environment
emptyEnvironment = Environment defaultFunctions Map.empty Map.empty Map.empty

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
isAvailable env name = not (Map.member name variables) && not (Map.member name types) && not (Map.member name cs)
    where variables = variableTypes env
          types = typeAliases env
          cs = constructors env

insertVariableType :: String -> Type -> Environment -> Environment
insertVariableType name t env = Environment newTypes (typeAliases env) (sumTypes env) (constructors env)
    where newTypes = Map.insert name t (variableTypes env)

insertTypeAlias :: String -> Type -> Environment -> Environment
insertTypeAlias name t env = Environment (variableTypes env) newTypes (sumTypes env) (constructors env)
    where newTypes = Map.insert name t (typeAliases env)

insertSumType :: String -> Map.Map String Type -> Environment -> Environment
insertSumType name t env = Environment (variableTypes env) (typeAliases env) newTypes (constructors env)
    where newTypes = Map.insert name t (sumTypes env)

insertConstructor :: String -> String -> Environment -> Environment
insertConstructor name t env = Environment (variableTypes env) (typeAliases env) (sumTypes env) newConstructors
    where newConstructors = Map.insert name t (constructors env)

instance Show TypeError where
    show (TypeError _ e) = e


hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates list = length list > Set.size (Set.fromList list)

allEqual :: Eq a => [a] -> Bool
allEqual [] = False
allEqual [_] = False
allEqual (x:y:xs) = if x == y then True else allEqual (y:xs)
