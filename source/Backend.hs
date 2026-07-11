{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Backend (compileProgram) where

import Closures
import LLVM

import Control.Monad.State (State, runState, get, put)
import Data.Map (Map, insert, lookup, empty)

compileProgram :: Closures.Program -> LLVM.Program
compileProgram program = LLVM.Program functions (statements s, e, llvmType $ getType mainExpression)
    where functions = map compileFunction (Closures.functions program)
          mainExpression = Closures.main program
          (e, s) = runState (compileExpression initialEnvironment mainExpression) initialState

compileFunction :: Closures.Function -> LLVM.Function
compileFunction function = LLVM.Function name argumentType returnType body
    where name = globalOperand (functionName function)
          argumentType = llvmType (Closures.argumentType function)
          returnType = llvmType (Closures.returnType function)
          body = statements state ++ [Return register returnType]
          (register, state) = runState (compileExpression env $ functionBody function) initialState
          env = initialEnvironmentWithType envType
          envType = LLVM.TupleType $ map llvmType (Closures.environmentType function)


compileExpression :: CompilationEnvironment -> Expression -> State CompilationState Operand
compileExpression env expression = case expression of
    Integer n -> do
        register <- reserveRegister
        putStatement $ integerLiteral register n
        return register

    Boolean b -> do
        register <- reserveRegister
        putStatement $ booleanLiteral register b
        return register

    Tuple members t -> do
        register <- allocate $ llvmTupleType t

        let insertMember :: (Int, Closures.Expression) -> State CompilationState ()
            insertMember (index, member) = do
                let memberType = llvmType $ getType member
                memberRegister <- compileExpression env member

                positionRegister <- reserveRegister
                putStatement $ GetElementPointer positionRegister (llvmTupleType t) register (integerOperand 0) (integerOperand index)
                putStatement $ Store memberRegister memberType positionRegister

        mapM_ insertMember (zip [0..] members)
        return register

    Closure definition environment _ -> do
        putStatement $ EmptyLine
        putStatement $ Comment ("Create a closure for " ++ show (globalOperand $ functionName definition))
        putStatement $ EmptyLine

        putStatement $ Comment "Allocate the necessary space"
        register <- allocate $ Backend.closureType
        let env' = case innermostLet env of
                Just s -> insertOverride s register env
                Nothing -> env
        putStatement $ EmptyLine

        putStatement $ Comment "Insert the function into the closure"
        positionRegister <- reserveRegister
        putStatement $ GetElementPointer positionRegister Backend.closureType register (integerOperand 0) (integerOperand 0)
        putStatement $ Store (globalOperand $ functionName definition) PointerType positionRegister
        putStatement $ EmptyLine

        -- The closure environment is essentially a tuple
        putStatement $ Comment "Create the closure environment"
        let tupleType = Closures.TupleType (map getType environment)
        environmentRegister <- compileExpression env' (Tuple environment tupleType)
        putStatement $ EmptyLine

        putStatement $ Comment "Insert the function into the closure"
        positionRegister <- reserveRegister
        putStatement $ GetElementPointer positionRegister Backend.closureType register (integerOperand 0) (integerOperand 1)
        putStatement $ Store environmentRegister PointerType positionRegister
        putStatement $ EmptyLine

        return register

    Argument _ ->
        return $ variableOperand "_argument"

    Local name _ -> do
        case Data.Map.lookup name (letOverrides env) of
            Just o -> return o
            Nothing -> return $ variableOperand name

    -- Retrieving a variable from the closure environment is essentially
    -- accessing a member of a special tuple
    Captured index t -> do
        let environment = variableOperand "_env"
        tupleMember environment (Backend.environmentType env) index (llvmType t)

    TupleMember index tuple t -> do
        tupleRegister <- compileExpression env tuple
        tupleMember tupleRegister (llvmTupleType $ getType tuple) index (llvmType t)

    If condition left right t -> do
        (leftLabel, rightLabel, mergeLabel) <- createBranch

        conditionRegister <- compileExpression env condition
        putStatement $ Branch conditionRegister leftLabel rightLabel

        putBasicBlock leftLabel
        putStatement $ Label leftLabel
        leftRegister <- compileExpression env left
        leftLabel <- getBasicBlock
        putStatement $ Jump mergeLabel

        putBasicBlock rightLabel
        putStatement $ Label rightLabel
        rightRegister <- compileExpression env right
        rightLabel <- getBasicBlock
        putStatement $ Jump mergeLabel

        putBasicBlock mergeLabel
        register <- reserveRegister
        putStatement $ Label mergeLabel
        putStatement $ Phi register (llvmType t) leftRegister leftLabel rightRegister rightLabel

        return register

    -- Built-in function calls
    Application (Application f@(BuiltInFunction name _) left _) right _ -> do
        leftRegister <- compileExpression env left
        rightRegister <- compileExpression env right

        register <- reserveRegister
        let opCode = case name of
                "+" -> Add
                "-" -> Sub
                "<" -> Slt
                ">" -> Sgt
                "<=" -> Sle
                ">=" -> Sge
                "==" -> Eq
                _ -> error ("unrecognized built-in operator " ++ name)

        let argumentType = case getType f of
                ClosureType argumentType _ -> argumentType
                _ -> error "found a built-in function whose type is not a closured type"

        putStatement $ BinaryOperation register (llvmType $ argumentType) opCode leftRegister rightRegister
        return register

    Application closure argument t -> do
        closureRegister <- compileExpression env closure

        -- Load the function from the closure
        positionRegister <- reserveRegister
        putStatement $ GetElementPointer positionRegister Backend.closureType closureRegister (integerOperand 0) (integerOperand 0)
        functionRegister <- reserveRegister
        putStatement $ Load functionRegister PointerType positionRegister

        -- Insert the environment into the closure
        positionRegister <- reserveRegister
        putStatement $ GetElementPointer positionRegister Backend.closureType closureRegister (integerOperand 0) (integerOperand 1)
        environmentRegister <- reserveRegister
        putStatement $ Load environmentRegister PointerType positionRegister

        argumentRegister <- compileExpression env argument

        resultRegister <- reserveRegister
        putStatement $ Call resultRegister (llvmType t) functionRegister environmentRegister argumentRegister (llvmType $ getType argument)
        return resultRegister

    Let name value body _ -> do
        let env' = insertLet name env
        valueRegister <- compileExpression env' value

        let variable = variableOperand name
        putStatement $ Bitcast variable (llvmType $ getType value) valueRegister (llvmType $ getType value)

        compileExpression env body

    _ -> error ("unexpected expression " ++ show expression)

allocate :: LLVM.Type -> State CompilationState Operand
allocate t = do
    sizePointerRegister <- reserveRegister
    putStatement $ GetElementPointerSimple sizePointerRegister t LLVM.null (integerOperand 1)

    sizeRegister <- reserveRegister
    putStatement $ PointerToInt sizeRegister sizePointerRegister

    resultRegister <- reserveRegister
    putStatement $ Malloc resultRegister sizeRegister

    return resultRegister

tupleMember :: Operand -> LLVM.Type -> Int -> LLVM.Type -> State CompilationState Operand
tupleMember tuple tupleType index t = do
    positionRegister <- reserveRegister
    putStatement $ GetElementPointer positionRegister tupleType tuple (integerOperand 0) (integerOperand index)

    resultRegister <- reserveRegister
    putStatement $ Load resultRegister t positionRegister
    return resultRegister

integerLiteral :: Operand -> Int -> Statement
integerLiteral operand n = BinaryOperation operand LLVM.IntegerType Add (integerOperand 0) (integerOperand n)

booleanLiteral :: Operand -> Bool -> Statement
booleanLiteral operand n = BinaryOperation operand LLVM.BooleanType Add (integerOperand 0) (booleanOperand n)
    where booleanOperand False = integerOperand 0
          booleanOperand True = integerOperand 1


-- State and environment utils

data CompilationState = CompilationState
    { register :: Int
    , branch :: Int
    , basicBlock :: Label
    , statements :: [Statement]
    }

initialState :: CompilationState
initialState = CompilationState 0 0 (MakeLabel "entry") []

reserveRegister :: State CompilationState Operand
reserveRegister = do
    state <- get
    let registerNumber = register state + 1
    put state { register = registerNumber }
    return $ registerOperand registerNumber

createBranch :: State CompilationState (Label, Label, Label)
createBranch = do
    state <- get
    let branchCount = branch state + 1
    put state { branch = branchCount }

    let leftLabel = MakeLabel $ "left" ++ show branchCount
        rightLabel = MakeLabel $ "right" ++ show branchCount
        mergeLabel = MakeLabel $ "merge" ++ show branchCount

    return (leftLabel, rightLabel, mergeLabel)

putBasicBlock :: Label -> State CompilationState ()
putBasicBlock bb = do
    state <- get
    put state { basicBlock = bb }

getBasicBlock :: State CompilationState Label
getBasicBlock = do
    state <- get
    return $ basicBlock state

putStatement :: Statement -> State CompilationState ()
putStatement s = do
    state <- get
    put state { statements = statements state ++ [s] }

data CompilationEnvironment = CompilationEnvironment
    { environmentType :: LLVM.Type -- the type of the function closure environment
    -- When compiling a recursive function, we need to refer to the function's
    -- closure before the function has been assigned to the final local
    -- variable. As such, whenever there is a let the innermostLet variable is
    -- set, and it will be used when a closure is created to point to the
    -- already existing register rather than the final local variable
    , innermostLet :: Maybe String
    , letOverrides :: Map String Operand
    }

initialEnvironment :: CompilationEnvironment
initialEnvironment = CompilationEnvironment LLVM.VoidType Nothing empty

initialEnvironmentWithType :: LLVM.Type -> CompilationEnvironment
initialEnvironmentWithType t = CompilationEnvironment t Nothing empty

insertLet :: String -> CompilationEnvironment -> CompilationEnvironment
insertLet s env = env { innermostLet = Just s }

insertOverride :: String -> Operand -> CompilationEnvironment -> CompilationEnvironment
insertOverride s o env = env { letOverrides = insert s o overrides }
    where overrides = letOverrides env

-- Type utils

-- Converts a type to its LLVM representation. Tuples in the high-level code are
-- represented as pointers in the LLVM IR.
llvmType :: Closures.Type -> LLVM.Type
llvmType Closures.IntegerType = LLVM.IntegerType
llvmType Closures.BooleanType = LLVM.BooleanType
llvmType (Closures.TupleType _) = LLVM.PointerType
llvmType (Closures.ClosureType _ _) = LLVM.PointerType

-- Despite the fact that variables holding tuples are represented with pointers
-- in the LLVM IR, to allocate them we need to use their tuple representation as
-- an argument to the get element pointer instruction. This function provides
-- the corresponding representation.
llvmTupleType :: Closures.Type -> LLVM.Type
llvmTupleType (Closures.TupleType ts) = LLVM.TupleType (map llvmType ts)
llvmTupleType _ = error "llvmTupleType should only be called on tuple types"

closureType :: LLVM.Type
closureType = LLVM.TupleType [PointerType, PointerType]
