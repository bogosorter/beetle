module Encloser (encloseProgram) where

import AST
import Closures (Program(..), Function(..))
import qualified Closures (Type(..), Expression(..), getType)

import Data.Set (Set, singleton, union, unions, difference, delete)
import qualified Data.Set as Set (empty, elems, fromList)
import Data.Map (Map, insert, findIndex, (!), findWithDefault)
import qualified Data.Map as Map (lookup, empty, elems, fromList)
import Control.Monad.State

encloseProgram :: TypedExpression -> Closures.Program
encloseProgram program = Program functions expression
    where (expression, ClosureState {Encloser.functions = functions}) =
            runState (enclose emptyEnvironment program) initialState


enclose :: Environment -> TypedExpression -> State ClosureState Closures.Expression
enclose env expression = case expression of
    Boolean {} -> return $ Closures.Boolean (booleanValue expression)
    Integer {} -> return $ Closures.Integer (integerValue expression)
    Character {} -> return $ Closures.Character (asciiValue expression)

    Variable {} -> do
        let name = variableName expression
        let vars = variables env

        case Map.lookup name vars of
            Just result -> return result
            Nothing -> case lookup name builtInFunctions of
                Just result -> return result
                Nothing -> error ("variable " ++ name ++ " should have been added to environment")

    Tuple {} -> do
        let members = tupleMembers expression
        enclosedMembers <- mapM (enclose env) members
        let enclosedType = Closures.TupleType $ map Closures.getType enclosedMembers
        return $ Closures.Tuple enclosedMembers enclosedType

    Record {} -> do
        let members = Map.elems $ recordMembers expression
        enclosedMembers <- mapM (enclose env) members
        let enclosedType = Closures.TupleType $ map Closures.getType enclosedMembers
        return $ Closures.Tuple enclosedMembers enclosedType

    Constructor {} -> do
        let sumType = AST.getType expression
            enclosedType = encloseType sumType

            constructors = getSumType env sumType
            index = findIndex (constructor expression) constructors

        enclosedValue <- enclose env $ value expression
        return $ Closures.Constructor enclosedValue index enclosedType

    Lowering {} -> do
        let Lowering value _ t = expression
        enclosedValue <- enclose env value
        return $ Closures.Lowering enclosedValue (encloseType t)

    TypeAssertion {} -> do
        let TypeAssertion scrutinee constructor _ = expression

        enclosedScrutinee <- enclose env scrutinee
        let constructors = getSumType env (getType scrutinee)
            index = findIndex constructor constructors

        return $ Closures.TypeAssertion enclosedScrutinee index

    AST.Function {} -> encloseFunction env expression Nothing

    If {} -> do
        enclosedCondition <- enclose env (condition expression)
        enclosedLeft <- enclose env (left expression)
        enclosedRight <- enclose env (right expression)
        return $ Closures.If enclosedCondition enclosedLeft enclosedRight (Closures.getType enclosedLeft)

    Application {} -> do
        enclosedFunction <- enclose env (function expression)
        enclosedArgument <- enclose env (argument expression)
        let enclosedType = encloseType (getType expression)
        return $ Closures.Application enclosedFunction enclosedArgument enclosedType

    RecordMember {} -> do
        enclosedRecord <- enclose env $ record expression
        let name = memberName expression
            memberTypes = case getType (record expression) of
                RecordType memberTypes -> memberTypes
                t -> error ("record member access on a variable whose type is " ++ show t)
            index = findIndex name memberTypes
            t = encloseType (memberTypes ! name)

        return $ Closures.TupleMember index enclosedRecord t

    TypeAssignment {} -> do
        let constructors = case (assignedType expression) of
                SumType constructors -> constructors
                _ -> error "only sum type assignments should make it past type checking"
            env' = insertSumType (assignedName expression) constructors env
        enclose env' (body expression)

    Assignment {} -> do
        -- If this is a function, we insert the variable into the environment even before enclosing its
        -- value to allow for recursion
        let env' = insertVariable name variable env
            name = assignedName expression
            variable = Closures.Local name (encloseType . getType . variableValue $ expression)

        -- If we are assigning a function, we want to give a name to it in the
        -- LLVM code, for it to be identifiable. We have to do it here, though,
        -- and not inside the function case, because this is where the
        -- information about the funtion name is.
        -- `env` or `env'` are chosen according to the type that we are enclosing.
        let expressionValue = variableValue expression
        enclosedValue <- case expressionValue of
            AST.Function {} -> encloseFunction env' expressionValue (Just name)
            _ -> enclose env (variableValue expression)

        enclosedBody <- enclose env' $ body expression
        return $ Closures.Let name enclosedValue enclosedBody (Closures.getType enclosedBody)

    TupleDestructuring {} -> do
        -- Tuple destructurings are transformed in to a series of lets of a
        -- single tuple member accesses. This simplifies the IR's logic and
        -- enables code sharing with record members.

        -- To separate the destructuring, we have to introduce a new variable to
        -- hold the tuple in-between member accesses.
        temporary <- createTemporary
        enclosedTuple <- enclose env (tuple expression)
        let tempReference = Closures.Local temporary (Closures.getType enclosedTuple)

        -- Create the environment to enclose the body before we create the let
        -- statements themselves - this is needed because to create the let
        -- statements we need to know the body type.
        let names = destructuredNames expression
            memberTypes = case Closures.getType enclosedTuple of
                Closures.TupleType memberTypes -> memberTypes
                _ -> error "tuples should only have tuple types"
            -- Holes should not be added to the environment
            variables = [Closures.Local name t | (name, t) <- zip names memberTypes]
            env' = foldr (uncurry insertVariable) env [pair | pair@(name, _) <- zip names variables, name /= "_"]

        -- Create the final value
        enclosedBody <- enclose env' (body expression)
        let bodyType = Closures.getType enclosedBody

        -- Create the enclosing tuple access statements
        let createLet name access body = Closures.Let name access body bodyType
            accesses = [Closures.TupleMember index tempReference t | (index, t) <- zip [0..] memberTypes]
            -- Accesses corresponding to holes should not be performed
            enclosedBody' = foldr (uncurry createLet) enclosedBody [pair | pair@(name, _) <- zip names accesses, name /= "_"]

        return $ Closures.Let temporary enclosedTuple enclosedBody' bodyType


encloseFunction :: Environment -> TypedExpression -> Maybe String -> State ClosureState Closures.Expression
encloseFunction env expression functionName = do
        let freeNames = Set.elems $ freeVariables expression
            freeValues = map (getVariable env) freeNames
            freeTypes = map Closures.getType freeValues

            -- The variables as they will be seen from within the closure
            variables = [Closures.Captured i t | (i, t) <- zip [0..] freeTypes]
            closureEnvironment = Environment (Map.fromList (zip freeNames variables)) (sumTypes env)

            -- The argument must also be added to the environment
            argName = argumentName expression
            argType = encloseType $ AST.argumentType expression
            retType = encloseType $ AST.returnType expression
            closureEnvironment' = insertVariable argName (Closures.Argument argType) closureEnvironment

        -- If name is nothing, we are enclosing a lambda
        name <- case functionName of
            Just name -> createFunction name
            Nothing -> createFunction "lambda"

        enclosedBody <- enclose closureEnvironment' (body expression)

        let definition = Closures.Function
                name
                argType
                retType
                freeTypes
                enclosedBody

        putFunction definition
        return $ Closures.Closure definition freeValues (encloseType $ getType expression)


freeVariables :: TypedExpression -> Set String
freeVariables expression = case expression of

    Boolean {} -> Set.empty
    Integer {} -> Set.empty
    Character {} -> Set.empty
    Variable { variableName = name } -> if name `elem` (map fst builtInFunctions) then Set.empty else singleton name

    Tuple {} ->
        unions $ map freeVariables (tupleMembers expression)

    Record { recordMembers = members } ->
        unions $ map freeVariables (Map.elems members)

    Constructor {} -> freeVariables (value expression)

    Lowering {} -> freeVariables (value expression)

    TypeAssertion {} -> freeVariables (scrutinee expression)

    AST.Function { argumentName = argument, body = body } ->
        delete argument $ freeVariables body

    If { condition = condition, left = left, right = right} ->
        freeVariables condition `union` freeVariables left `union` freeVariables right

    Application { function = function, argument = argument} ->
        freeVariables function `union` freeVariables argument

    RecordMember { record = record } -> freeVariables record

    Assignment { assignedName = name, variableValue = value, body = body} ->
        -- To allow for expressions like `x = x + 1`, self-references in the
        -- value of an assignment are considered free as long as this is not a
        -- function assignment (which allows for recursion)
        case value of
            AST.Function {} -> delete name $ freeVariables value `union` freeVariables body
            _ -> freeVariables value `union` (delete name $ freeVariables body)

    TupleDestructuring { destructuredNames = names, tuple = tuple, body = body} ->
        (freeVariables tuple `union` freeVariables body) `difference` (Set.fromList names)

    TypeAssignment { body = body } -> freeVariables body


encloseType :: Type -> Closures.Type
encloseType BooleanType = Closures.BooleanType
encloseType IntegerType = Closures.IntegerType
encloseType CharacterType = Closures.CharacterType
encloseType (TupleType memberTypes) = Closures.TupleType (map encloseType memberTypes)
encloseType (RecordType memberTypes) = Closures.TupleType (map encloseType $ Map.elems memberTypes)
encloseType (FunctionType argumentType returnType) = Closures.ClosureType (encloseType argumentType) (encloseType returnType)
-- All type aliases are removed by now, and only sum types are left
encloseType (UserType _) = Closures.SumType
encloseType (SumType _) = error "sum types should have been removed in type checking"
encloseType (TypeVariable _) = error "type variables should have been removed in type checking"
encloseType (TypeVariableInstance _) = error "type variable instances should have been removed in type checking"


-- State and environment utils

data ClosureState = ClosureState
    { functions :: [Function]
    , functionCounter :: Map String Int
    , temporary :: Int
    }

initialState :: ClosureState
initialState = ClosureState [] Map.empty 0

createTemporary :: State ClosureState String
createTemporary = do
    state <- get
    let result = temporary state
    put state { temporary = result + 1}
    return $ "_tmp" ++ show result

createFunction :: String -> State ClosureState String
createFunction name = do
    state <- get
    let counter = functionCounter state
    let functionCount = findWithDefault 0 name counter + 1
    put $ state { functionCounter = insert name functionCount counter }
    return $ name ++ "_" ++ show functionCount

putFunction :: Function -> State ClosureState ()
putFunction f = do
    state <- get
    let fs = Encloser.functions state
    put state { Encloser.functions = fs ++ [f] }

data Environment = Environment
    { variables :: Map String Closures.Expression
    , sumTypes :: Map String (Map String AST.Type)
    }

emptyEnvironment :: Environment
emptyEnvironment = Environment Map.empty Map.empty

insertVariable :: String -> Closures.Expression -> Environment -> Environment
insertVariable name variable env = env { variables = variables' }
    where variables' = insert name variable $ variables env

getVariable :: Environment -> String -> Closures.Expression
getVariable env name = variables env ! name

insertSumType :: String -> Map String AST.Type -> Environment -> Environment
insertSumType name t env = env { sumTypes = sumTypes' }
    where sumTypes' = insert name t $ sumTypes env

getSumType :: Environment -> AST.Type -> Map String AST.Type
getSumType env constructor = case constructor of
    UserType name -> case Map.lookup name (sumTypes env) of
        Just t -> t
        Nothing -> error ("couldn't find name " ++ name)
    _ -> error ("got a constructor whose type is not sum type: " ++ show constructor)

builtInFunctions :: [(String, Closures.Expression)]
builtInFunctions =
    [ makeBuiltInFunction "*" Closures.IntegerType Closures.IntegerType Closures.IntegerType
    , makeBuiltInFunction "/" Closures.IntegerType Closures.IntegerType Closures.IntegerType
    , makeBuiltInFunction "mod" Closures.IntegerType Closures.IntegerType Closures.IntegerType
    , makeBuiltInFunction "rem" Closures.IntegerType Closures.IntegerType Closures.IntegerType
    , makeBuiltInFunction "+" Closures.IntegerType Closures.IntegerType Closures.IntegerType
    , makeBuiltInFunction "-" Closures.IntegerType Closures.IntegerType Closures.IntegerType
    , makeBuiltInFunction "<" Closures.IntegerType Closures.IntegerType Closures.BooleanType
    , makeBuiltInFunction ">" Closures.IntegerType Closures.IntegerType Closures.BooleanType
    , makeBuiltInFunction "<=" Closures.IntegerType Closures.IntegerType Closures.BooleanType
    , makeBuiltInFunction ">=" Closures.IntegerType Closures.IntegerType Closures.BooleanType
    , makeBuiltInFunction "==" Closures.IntegerType Closures.IntegerType Closures.BooleanType
    , ("not", Closures.BuiltInFunction "not" (Closures.ClosureType Closures.BooleanType Closures.BooleanType))
    ]

makeBuiltInFunction :: String -> Closures.Type -> Closures.Type -> Closures.Type -> (String, Closures.Expression)
makeBuiltInFunction name left right result = (name, Closures.BuiltInFunction name (Closures.ClosureType left (Closures.ClosureType right result)))
