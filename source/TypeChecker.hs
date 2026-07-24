{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module TypeChecker (typeCheckProgram, TypeError(..)) where

import AST

import Text.Megaparsec (SourcePos)
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.Set as Set
import Control.Monad.State (StateT, runStateT, get, put, lift)
import Control.Monad (unless, when)


data TypeError = TypeError SourcePos String
data Constraint = Constraint Type Type SourcePos String

typeCheckProgram :: SourceExpression -> Either [TypeError] TypedExpression
typeCheckProgram program = do
    -- Type-checking is divided into two phases: the first one generates either
    -- an error that forces the type-checking to a halt or a list of
    -- constraints. These constraints are used in the second phase to generate a
    -- list of type errors. This allows for the reporting of multiple type
    -- errors.

    -- First phase
    (typedProgram, constraints) <- case runStateT (typeCheck emptyEnvironment program) [] of
        Left typeError -> Left [typeError]
        Right result -> Right $ result

    -- Second phase
    enforce constraints

    -- Ensure that the ouptut of the program is one of the allowed types
    case getType typedProgram of
        IntegerType -> Right typedProgram
        BooleanType -> Right typedProgram
        CharacterType -> Right typedProgram
        UserType "String" -> Right typedProgram
        _ -> Left $ [TypeError (getPosition program) "every program must return an integer, a boolean, a character or a string"]


-- This section deals with the generation of type constraints

type Generator = StateT [Constraint] (Either TypeError)

typeCheck :: Environment -> SourceExpression -> Generator TypedExpression
typeCheck env expression = case expression of
    Integer {} -> return $ Integer (integerValue expression) IntegerType
    Boolean {} -> return $ Boolean (booleanValue expression) BooleanType
    Character {} -> return $ Character (asciiValue expression) CharacterType

    Tuple {} -> do
        typedMembers <- mapM (typeCheck env) (tupleMembers expression)
        let tupleType = TupleType $ map getType typedMembers
        return $ Tuple typedMembers tupleType

    Record {} -> do
        typedMembers <- mapM (typeCheck env) (recordMembers expression)
        let tupleType = RecordType $ Map.map getType typedMembers
        return $ Record typedMembers tupleType

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

                putConstraint $ Constraint desugaredExpectedType valueType (getPosition $ value expression)  ("expected type " ++ show expectedValueType ++ ", but got value of type " ++ show valueType)
                return $ Constructor name typedValue (UserType sumType)

            Nothing -> lift $ Left $ TypeError position ("couldn't find constructor " ++ name)

    Lowering {} -> do
        let Lowering value constructor _ = expression

        typedValue <- typeCheck env value
        let loweredType = case getType typedValue of
                UserType userType -> case Map.lookup userType (sumTypes env) of
                    Just constructors -> constructors ! constructor
                    _ -> error ("the value of a lowering must be a sum type, but got type " ++ userType)
                t -> error ("the value of a lowering must be a sum type, but got type " ++ show t)

        return $ Lowering typedValue constructor loweredType

    Function {} -> do
        let Function argumentType returnType argumentName body _ = expression
        argumentType <- desugar position env argumentType
        returnType <- desugar position env returnType

        let env' = insertVariableType argumentName argumentType env
        typedBody <- typeCheck env' body

        putConstraint $ Constraint (getType typedBody) returnType position "function return type does not match body type"
        return $ Function argumentType returnType argumentName typedBody (FunctionType argumentType returnType)

    If { condition = TypeAssertion {} } -> do
        let If (TypeAssertion scrutinee constructor assertionPosition) left right position = expression

        typedScrutinee <- typeCheck env scrutinee
        constructors <- case getType typedScrutinee of
            UserType userType -> case Map.lookup userType (sumTypes env) of
                Just constructors -> return constructors
                _ -> lift $ Left $ TypeError assertionPosition ("the scrutinee of a type assertion must be a sum type, but got type " ++ userType)
            t -> lift $ Left $ TypeError assertionPosition ("the scrutinee of a type assertion must be a sum type, but got type " ++ show t)

        unless (Map.member constructor constructors) $
            lift $ Left $ TypeError position ("constructor " ++ show constructor ++ " does not exist in type " ++ show (getType typedScrutinee))


        case scrutinee of
            (Variable name _) -> when (Set.member constructor (getImpossibleConstructors name env)) $
                lift $ Left $ TypeError position ("redundant check: it has already been established that \"" ++ name ++ "\" is not of type " ++ constructor)
            _ -> return ()

        -- If the scrutinee is a variable, we can change the type of that
        -- variable, and possibly change the type of the variable on the else
        -- branch, if it is known that there is a single possibility left.
        let (left', right', rightEnv) = case scrutinee of
                (Variable name _) ->
                    let modifiedLeft = Assignment name (Lowering scrutinee constructor position) left position
                        env' = insertImpossibleConstructor name constructor env
                        missingConstructors = Map.keys $ Map.withoutKeys constructors (getImpossibleConstructors name env')
                        modifiedRight = case missingConstructors of
                            [x] ->  Assignment name (Lowering scrutinee x position) right position
                            _ -> right
                    in (modifiedLeft, modifiedRight, env')
                _ -> (left, right, env)

        typedLeft <- typeCheck env left'
        typedRight <- typeCheck rightEnv right'

        putConstraint $ Constraint (getType typedLeft) (getType typedRight) position "the branches of an if expression must have the same return type"
        return $ If (TypeAssertion typedScrutinee constructor BooleanType) typedLeft typedRight (getType typedLeft)

    If {} -> do
        typedCondition <- typeCheck env (condition expression)
        putConstraint $ Constraint (getType typedCondition) BooleanType position "the condition of an if must always be a boolean expression"

        typedLeft <- typeCheck env (left expression)
        typedRight <- typeCheck env (right expression)

        putConstraint $ Constraint (getType typedLeft) (getType typedRight) position "the branches of an if expression must have the same return type"
        return $ If typedCondition typedLeft typedRight (getType typedLeft)

    Application {} -> do
        typedFunction <- typeCheck env (function expression)
        (argumentType, returnType) <- case getType typedFunction of
            FunctionType argumentType returnType -> return $ (argumentType, returnType)
            t -> lift $ Left $ TypeError position ("can only apply functions, but got type " ++ show t)

        typedArgument <- typeCheck env (argument expression)
        -- We do not overwrite the initial variable to keep user-defined
        -- variables in the error output
        argumentType' <- desugar position env argumentType
        returnType <- desugar position env returnType

        putConstraint $ Constraint (getType typedArgument) argumentType' (getPosition $ argument expression) ("expected an argument of type " ++ show argumentType ++ ", but got argument of type " ++ show (getType typedArgument))
        return $ Application typedFunction typedArgument returnType

    Variable {} -> do
        let name = variableName expression
        let types = variableTypes env
        case Map.lookup name types of
            Just t -> do
                t <- desugar position env t
                return $ Variable name t
            _ -> lift $ Left $ TypeError position ("couldn't find name " ++ name)

    RecordMember {} -> do
        typedRecord <- typeCheck env (record expression)

        case getType typedRecord of
            UserType name -> typeCheckSumRecordMember expression name env
            RecordType memberTypes ->
                let name = memberName expression
                in case Map.lookup name memberTypes of
                    Just t -> return $ RecordMember typedRecord name t
                    _ -> lift $ Left $ TypeError position ("trying to access inexistent member " ++ name ++ " of this record")
            t -> lift $ Left $ TypeError position ("can only access members of records, but got type " ++ show t)

    -- Type shadowing is not allowed
    TypeAssignment {} -> do
        let name = assignedName expression
        unless (isAvailable env name) $
            lift $ Left $ TypeError position ("type " ++ name ++ " has already been declared")

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
            lift $ Left $ TypeError position ("assigning to a hole")

        -- Clear the previous data about this variable, if any
        let env' = clearImpossibleConstructors name env

        -- For the moment, arbitrary recursion is not allowed. For instance, a
        -- lambda cannot refer to the variable that holds it. This can only
        -- happen if there is an assignment whose direct child is a function
        -- (whose type is know a priori). As such, if this is such a function
        -- its type is inserted before evaluation, if not it is inserted after
        -- evaluation
        (typedValue, env'') <- case (variableValue expression) of

            Function { argumentType = at, returnType = rt } -> do
                let env'' = insertVariableType name (FunctionType at rt) env'
                typedValue <- typeCheck env'' (variableValue expression)
                return (typedValue, env'')

            _ -> do
                typedValue <- typeCheck env' (variableValue expression)
                let env'' = insertVariableType name (getType typedValue) env'
                return (typedValue, env'')


        typedBody <- typeCheck env'' (body expression)
        return $ Assignment name typedValue typedBody (getType typedBody)

    TupleDestructuring {} -> do
        let names = destructuredNames expression

        typedTuple <- typeCheck env (tuple expression)
        memberTypes <- case getType typedTuple of
            TupleType memberTypes -> return $ memberTypes
            t -> lift $ Left $ TypeError position ("attempting to destructure tuple, but got type " ++ show t)

        when (length names /= length memberTypes) $
            lift $ Left $ TypeError position ("the number of assigned variables does not match the number of tuple members")

        let env' = foldr (uncurry insertVariableType) env (zip names memberTypes)
        typedBody <- typeCheck env' (body expression)

        return $ TupleDestructuring names typedTuple typedBody (getType typedBody)

    TypeAssertion {} -> error "type assertions can only appear within ifs"

    where position = getPosition expression


-- When all of the remaining constructors of a sum type variable are of record
-- type and all share a member of the same type, that member can be accessed
-- directly
typeCheckSumRecordMember :: SourceExpression -> String -> Environment -> Generator TypedExpression
typeCheckSumRecordMember expression userType env = do
    let RecordMember record memberName position = expression

    constructors <- case Map.lookup userType (sumTypes env) of
        Just constructors -> case record of
            (Variable name _) -> return $ Map.withoutKeys constructors (getImpossibleConstructors name env)
            _ -> return constructors
        _ -> lift $ Left $ TypeError position ("can only access members of records, but got type " ++ show userType)

    let ensureIsRecord :: String -> Type -> Generator (String, Map.Map String Type)
        ensureIsRecord constructor t  = case t of
            RecordType members -> return (constructor, members)
            _ -> lift $ Left $ TypeError position ("can only access members of records, but " ++ constructor ++ " is of type " ++ show t)

    records <- mapM (uncurry ensureIsRecord) (Map.toList constructors)

    let ensureHasMember :: String -> Map.Map String Type -> Generator Type
        ensureHasMember name memberTypes = case Map.lookup memberName memberTypes of
            Just t -> return t
            Nothing -> lift $ Left $ TypeError position ("type " ++ name ++ " does not have a member \"" ++ memberName ++ "\"")

    memberTypes <- mapM (uncurry ensureHasMember) records
    unless (allEqual memberTypes) $
        lift $ Left $ TypeError position ("there are multiple possible types for the value of \"" ++ memberName ++ "\" in " ++ intercalate " | " (Map.keys constructors))

    -- To access the member, each of the possible types has to be checked, and,
    -- if it is that one, a lowering followed by a record access must be
    -- performed.

    let -- If the record is a variable, on the if branches its type will have
        -- already been lowered, which would create an error. As such, in that
        -- case it is not necessary to perform a lowering.
        lower :: String -> SourceExpression
        lower constructor = case record of
            (Variable _ _) -> record
            _ -> (Lowering record constructor position)

        addConstructor :: String -> SourceExpression -> SourceExpression
        addConstructor constructor rightBranch =
            If (TypeAssertion record constructor position)
                (RecordMember (lower constructor) memberName position)
                rightBranch
                position

        (lastConstructor, _) = last records
        lastAccess = RecordMember (lower lastConstructor) memberName position
        access = foldr addConstructor lastAccess [constructor | (constructor, _) <- init records]

    typeCheck env access

desugar :: SourcePos -> Environment -> Type -> Generator Type
desugar position env (UserType name) = do
    let aliases = typeAliases env
    case Map.lookup name aliases of
        Just t -> desugar position env t
        Nothing -> do
            let sums = sumTypes env
            case Map.lookup name sums of
                Just _ -> return $ UserType name
                Nothing -> lift $ Left $ TypeError position ("couldn't find type " ++ name)
desugar position env (TupleType memberTypes) = do
    desugaredMemberTypes <- mapM (desugar position env) memberTypes
    return $ TupleType desugaredMemberTypes
desugar position env (RecordType memberTypes) = do
    desugaredMemberTypes <- mapM (desugar position env) memberTypes
    return $ RecordType desugaredMemberTypes
desugar position env (FunctionType argumentType returnType) = do
    desugaredArgumentType <- desugar position env argumentType
    desugaredReturnType <- desugar position env returnType
    return $ FunctionType desugaredArgumentType desugaredReturnType
desugar _ _ t = return $ t


-- This section deals with enforcing type constraints

enforce :: [Constraint] -> Either [TypeError] ()
enforce constraints = case solve constraints of
    [] -> Right ()
    errors -> Left errors

solve :: [Constraint] -> [TypeError]
solve [] = []
solve (constraint:constraints)
    | a == b = solve constraints
    | otherwise = TypeError position message : solve constraints
    where Constraint a b position message = constraint




-- Environment utils

data Environment = Environment
    { variableTypes :: Map.Map String Type
    , typeAliases :: Map.Map String Type
    , sumTypes :: Map.Map String (Map.Map String Type)
    , constructors :: Map.Map String String
    -- Impossible constructors of a given variable are sum type branches that
    -- have already been rulled out by previous if is instances. Once these
    -- reach n - 1 constructors, the variable can only have one possible type
    -- and may be lowered.
    , impossibleConstructors :: Map.Map String (Set.Set String)
    }

emptyEnvironment :: Environment
emptyEnvironment = Environment defaultFunctions Map.empty Map.empty Map.empty Map.empty

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
insertVariableType name t env = env { variableTypes = newTypes }
    where newTypes = Map.insert name t (variableTypes env)

insertTypeAlias :: String -> Type -> Environment -> Environment
insertTypeAlias name t env = env { typeAliases = newTypes }
    where newTypes = Map.insert name t (typeAliases env)

insertSumType :: String -> Map.Map String Type -> Environment -> Environment
insertSumType name t env = env { sumTypes = newTypes }
    where newTypes = Map.insert name t (sumTypes env)

insertConstructor :: String -> String -> Environment -> Environment
insertConstructor name t env = env { constructors = newConstructors }
    where newConstructors = Map.insert name t (constructors env)

insertImpossibleConstructor :: String -> String -> Environment -> Environment
insertImpossibleConstructor name constructor env = env { impossibleConstructors = newImpossible }
    where newImpossible = Map.insert name newConstructors (impossibleConstructors env)
          newConstructors = Set.insert constructor (Map.findWithDefault Set.empty name (impossibleConstructors env))

getImpossibleConstructors :: String -> Environment -> Set.Set String
getImpossibleConstructors name env = case Map.lookup name (impossibleConstructors env) of
    Just result -> result
    Nothing -> Set.empty

clearImpossibleConstructors :: String -> Environment -> Environment
clearImpossibleConstructors name env = env { impossibleConstructors = newImpossible }
    where newImpossible = Map.delete name (impossibleConstructors env)

putConstraint :: Constraint -> Generator ()
putConstraint constraint = do
    constraints <- get
    put $ constraints ++ [constraint]

instance Show TypeError where
    show (TypeError _ e) = e


-- Utils
allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual [_] = True
allEqual (x:y:zs) = if x /= y then False else allEqual (y:zs)
