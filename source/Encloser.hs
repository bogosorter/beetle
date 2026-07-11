{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Encloser (encloseProgram) where

import AST
import Closures (Program(..), Function(..))
import qualified Closures (Type(..), Expression(..), getType)

import Data.Set (Set, singleton, union, unions, difference, delete)
import qualified Data.Set as Set (empty, elems, fromList)
import Data.Map (Map, insert, findIndex, (!))
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

    Variable {} -> do
        let name = variableName expression
        let vars = variables env

        case Map.lookup name vars of
            Just result -> return result
            Nothing -> case lookup name builtInFunctions of
                Just result -> return result
                Nothing -> error "variable should have been added to environment"

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

    Assignment {} -> do
        -- We insert the variable into the environment even before enclosing its
        -- value to allow for recursion
        let env' = insertVariable name variable env
            variable = Closures.Local (variableName expression) (encloseType . getType . variableValue $ expression)
            name = variableName expression

        enclosedValue <- enclose env' (variableValue expression)

        -- If we are assigning a function, we want to give a name to it in the
        -- LLVM code, for it to be identifiable. We have to do it here, though,
        -- and not inside the function case, because this is where the
        -- information about the funtion name is
        let expressionBody = body expression
        enclosedBody <- case expressionBody of
            AST.Function {} -> encloseFunction env' expressionBody (Just name)
            _ -> enclose env' expressionBody

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
            variables = map (\(name, t) -> Closures.Local name t) (zip names memberTypes)
            env' = foldr (uncurry insertVariable) env (zip names variables)

        -- Create the final value
        enclosedBody <- enclose env' (body expression)
        let bodyType = Closures.getType enclosedBody

        -- Create the enclosing tuple access statements
        let createLet name access body = Closures.Let name access body bodyType
            accesses = map (\(index, t) -> Closures.TupleMember index tempReference t) (zip [0..] memberTypes)
            enclosedBody' = foldr (uncurry createLet) enclosedBody (zip names accesses)

        return $ Closures.Let temporary enclosedTuple enclosedBody' bodyType

    TypeDeclaration {} -> error "type variables should have been removed in type checking"


encloseFunction :: Environment -> TypedExpression -> Maybe String -> State ClosureState Closures.Expression
encloseFunction env expression functionName = do
        let freeNames = Set.elems $ freeVariables expression
            freeValues = map (getVariable env) freeNames
            freeTypes = map Closures.getType freeValues

            -- The variables as they will be seen from within the closure
            variables = [Closures.Captured i t | (i, t) <- zip [0..] freeTypes]
            closureEnvironment = fromVariables $ Map.fromList (zip freeNames variables)

            -- The argument must also be added to the environment
            argName = argumentName expression
            argType = encloseType $ AST.argumentType expression
            retType = encloseType $ AST.returnType expression
            closureEnvironment' = insertVariable argName (Closures.Argument argType) closureEnvironment

        -- If name is nothing, we are enclosing a lambda
        name <- case functionName of
            Just n -> return n
            Nothing -> createLambda

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
    Variable { variableName = name } -> if name `elem` (map fst builtInFunctions) then Set.empty else singleton name

    Tuple { tupleMembers = members } ->
        unions $ map freeVariables members

    Record { recordMembers = members } ->
        unions $ map freeVariables (Map.elems members)

    AST.Function { argumentName = argument, body = body } ->
        delete argument $ freeVariables body

    If { condition = condition, left = left, right = right} ->
        freeVariables condition `union` freeVariables left `union` freeVariables right

    Application { function = function, argument = argument} ->
        freeVariables function `union` freeVariables argument

    RecordMember { record = record } -> freeVariables record

    Assignment { variableName = name, variableValue = value, body = body} ->
        delete name (freeVariables value `union` freeVariables body)

    TupleDestructuring { destructuredNames = names, tuple = tuple, body = body} ->
        (freeVariables tuple `union` freeVariables body) `difference` (Set.fromList names)

    TypeDeclaration {} -> error "type variables should have been removed in type checking"


encloseType :: Type -> Closures.Type
encloseType BooleanType = Closures.BooleanType
encloseType IntegerType = Closures.IntegerType
encloseType (TupleType memberTypes) = Closures.TupleType (map encloseType memberTypes)
encloseType (RecordType memberTypes) = Closures.TupleType (map encloseType $ Map.elems memberTypes)
encloseType (FunctionType argumentType returnType) = Closures.ClosureType (encloseType argumentType) (encloseType returnType)
encloseType (UserType _) = error "type variables should have been removed in type checking"


-- State and environment utils

data ClosureState = ClosureState
    { functions :: [Function]
    , temporary :: Int
    , lambda :: Int
    }

initialState :: ClosureState
initialState = ClosureState [] 0 0

createTemporary :: State ClosureState String
createTemporary = do
    state <- get
    let result = temporary state
    put state { temporary = result + 1}
    return $ "_tmp" ++ show result

createLambda :: State ClosureState String
createLambda = do
    state <- get
    let result = lambda state
    put $ state { lambda = result + 1}
    return $ "_lambda" ++ show result

putFunction :: Function -> State ClosureState ()
putFunction f = do
    state <- get
    let fs = Encloser.functions state
    put state { Encloser.functions = f : fs }

data Environment = Environment
    { variables :: Map String Closures.Expression
    }

emptyEnvironment :: Environment
emptyEnvironment = Environment Map.empty

fromVariables :: Map String Closures.Expression -> Environment
fromVariables variables = Environment variables

insertVariable :: String -> Closures.Expression -> Environment -> Environment
insertVariable name variable env = env { variables = variables' }
    where variables' = insert name variable $ variables env

getVariable :: Environment -> String -> Closures.Expression
getVariable env name = variables env ! name

builtInFunctions :: [(String, Closures.Expression)]
builtInFunctions =
    [ makeBuiltInFunction "+" Closures.IntegerType Closures.IntegerType Closures.IntegerType
    , makeBuiltInFunction "-" Closures.IntegerType Closures.IntegerType Closures.IntegerType
    , makeBuiltInFunction "<" Closures.IntegerType Closures.IntegerType Closures.BooleanType
    , makeBuiltInFunction ">" Closures.IntegerType Closures.IntegerType Closures.BooleanType
    , makeBuiltInFunction "<=" Closures.IntegerType Closures.IntegerType Closures.BooleanType
    , makeBuiltInFunction ">=" Closures.IntegerType Closures.IntegerType Closures.BooleanType
    , makeBuiltInFunction "==" Closures.IntegerType Closures.IntegerType Closures.BooleanType
    ]

makeBuiltInFunction :: String -> Closures.Type -> Closures.Type -> Closures.Type -> (String, Closures.Expression)
makeBuiltInFunction name left right result = (name, Closures.BuiltInFunction name (Closures.ClosureType left (Closures.ClosureType right result)))
