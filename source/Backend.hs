module Backend (compileProgram) where

import Closures
import LLVM

import Control.Monad.State (State, runState, get, put)
import Data.Map (Map, insert, empty, (!), findWithDefault)

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
          envType = LLVM.TupleType $ map (\_ -> BoxedType) (Closures.environmentType function)


compileExpression :: CompilationEnvironment -> Expression -> State CompilationState Operand
compileExpression env expression = case expression of
    Integer n -> return $ integerOperand n
    Boolean b -> return $ booleanOperand b
    Character c -> return $ characterOperand c

    Tuple members t -> do
        putStatement $ Comment "Allocating space for tuple"
        register <- allocate $ llvmTupleType t

        putStatement $ Comment "Calculating and inserting members"

        let insertMember :: (Int, Closures.Expression) -> State CompilationState ()
            insertMember (index, member) = do
                let memberType = llvmType $ getType member
                memberRegister <- compileExpression env member
                -- Tuple members are boxed to facilitate polymorphism management
                boxedRegister <- box memberRegister memberType

                positionRegister <- reserveRegister
                putStatement $ GetElementPointer positionRegister (llvmTupleType t) register (integerOperand 0) (integerOperand index)
                putStatement $ Store boxedRegister BoxedType positionRegister

        mapM_ insertMember (zip [0..] members)
        return register

    Closure definition environment _ -> do
        putStatement $ EmptyLine
        putStatement $ Comment ("Create a closure for " ++ show (globalOperand $ functionName definition))

        putStatement $ Comment "Allocate the necessary space"
        register <- allocate $ Backend.closureType
        let env' = case innermostLet env of
                Just s -> insertVariable s register env
                Nothing -> env

        putStatement $ Comment "Insert the function into the closure"
        positionRegister <- reserveRegister
        putStatement $ GetElementPointer positionRegister Backend.closureType register (integerOperand 0) (integerOperand 0)
        putStatement $ Store (globalOperand $ functionName definition) PointerType positionRegister

        -- The closure environment is essentially a tuple
        putStatement $ Comment "Create the closure environment"
        let tupleType = Closures.TupleType (map getType environment)
        environmentRegister <- compileExpression env' (Tuple environment tupleType)

        putStatement $ Comment "Insert the function into the closure"
        positionRegister <- reserveRegister
        putStatement $ GetElementPointer positionRegister Backend.closureType register (integerOperand 0) (integerOperand 1)
        putStatement $ Store environmentRegister PointerType positionRegister
        putStatement $ EmptyLine

        return register

    Argument _ ->
        return $ variableOperand "_argument"

    Local name _ ->
        return $ variableOperands env ! name

    -- Retrieving a variable from the closure environment is essentially
    -- accessing a member of a special tuple
    Captured index t -> do
        let environment = variableOperand "_env"
        result <- tupleMember environment (Backend.environmentType env) index (llvmType t)
        return result

    TupleMember index tuple t -> do
        putStatement $ Comment "Accessing tuple member"
        tupleRegister <- compileExpression env tuple
        tupleMember tupleRegister (llvmTupleType $ getType tuple) index (llvmType t)

    Constructor value index _ -> do
        valueRegister <- compileExpression env value

        putStatement $ Comment "Lifting to sum type"
        resultRegister <- allocate sumType

        constructorRegister <- reserveRegister
        putStatement $ GetElementPointer constructorRegister sumType resultRegister (integerOperand 0) (integerOperand 0)
        putStatement $ Store (integerOperand index) LLVM.IntegerType constructorRegister

        positionRegister <- reserveRegister
        putStatement $ GetElementPointer positionRegister sumType resultRegister (integerOperand 0) (integerOperand 1)
        putStatement $ Store valueRegister (llvmType $ getType value) positionRegister

        return resultRegister

    Lowering value t -> do
        valueRegister <- compileExpression env value

        putStatement $ Comment "Lowering sum type"
        positionRegister <- reserveRegister
        resultRegister <- reserveRegister
        putStatement $ GetElementPointer positionRegister sumType valueRegister (integerOperand 0) (integerOperand 1)
        putStatement $ Load resultRegister (llvmType t) positionRegister

        return resultRegister

    TypeAssertion scrutinee index -> do
        scrutineeRegister <- compileExpression env scrutinee

        putStatement $ Comment "Type assertion"
        putStatement $ Comment "Extracting the constructor of a sum type"
        constructorPosition <- reserveRegister
        constructor <- reserveRegister
        putStatement $ GetElementPointer constructorPosition sumType scrutineeRegister (integerOperand 0) (integerOperand 0)
        putStatement $ Load constructor LLVM.IntegerType constructorPosition

        putStatement $ Comment "Comparing with the expected constructor"
        resultRegister <- reserveRegister
        putStatement $ BinaryOperation resultRegister LLVM.IntegerType Eq constructor (integerOperand index)

        return resultRegister

    If condition left right t -> do
        (leftLabel, rightLabel, mergeLabel) <- createBranch

        conditionRegister <- compileExpression env condition
        putStatement $ Branch conditionRegister leftLabel rightLabel
        putStatement $ EmptyLine

        putLabel leftLabel
        leftRegister <- compileExpression env left
        leftLabel <- getBasicBlock
        putStatement $ Jump mergeLabel
        putStatement $ EmptyLine

        putLabel rightLabel
        rightRegister <- compileExpression env right
        rightLabel <- getBasicBlock
        putStatement $ Jump mergeLabel
        putStatement $ EmptyLine

        putLabel mergeLabel
        register <- reserveRegister
        putStatement $ Phi register (llvmType t) [(leftRegister, leftLabel), (rightRegister, rightLabel)]
        putStatement $ EmptyLine

        return register

    -- Built-in not function call
    Application (BuiltInFunction "not" _) argument _ -> do
        argumentRegister <- compileExpression env argument

        register <- reserveRegister
        putStatement $ BinaryOperation register LLVM.BooleanType Xor argumentRegister (integerOperand (-1))
        return register

    -- Built-in function calls
    Application (Application f@(BuiltInFunction name _) left _) right _ -> do
        leftRegister <- compileExpression env left
        rightRegister <- compileExpression env right

        register <- reserveRegister
        let opCode = case name of
                "*" -> Mul
                "/" -> Div
                "rem" -> Rem
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
        let (argumentType, returnType) = case getType closure of
                ClosureType argumentType returnType -> (argumentType, returnType)
                _ -> error "got a closure whose type is not closure type"

        putStatement $ Comment "Function Application"

        closureRegister <- compileExpression env closure

        putStatement $ Comment "Load the function from the closure"
        positionRegister <- reserveRegister
        putStatement $ GetElementPointer positionRegister Backend.closureType closureRegister (integerOperand 0) (integerOperand 0)
        functionRegister <- reserveRegister
        putStatement $ Load functionRegister PointerType positionRegister

        putStatement $ Comment "Load the environment from the closure"
        positionRegister <- reserveRegister
        putStatement $ GetElementPointer positionRegister Backend.closureType closureRegister (integerOperand 0) (integerOperand 1)
        environmentRegister <- reserveRegister
        putStatement $ Load environmentRegister PointerType positionRegister

        putStatement $ Comment "Calculate the argument"
        argumentRegister <- compileExpression env argument

        -- If this argument is a generic variable, we need to cast the value to
        -- a standard i64 type
        boxedArgument <- case argumentType of
            GenericType -> box argumentRegister (llvmType $ getType argument)
            _ -> return argumentRegister

        boxedResult <- reserveRegister
        putStatement $ Comment "Perform the function call"
        putStatement $ Call boxedResult (llvmType returnType) functionRegister environmentRegister boxedArgument (llvmType argumentType)

        resultRegister <- case returnType of
            GenericType -> unbox boxedResult (llvmType t)
            _ -> return boxedResult
        putStatement $ EmptyLine

        return resultRegister

    Let name value body _ -> do
        let env' = insertLet name env
        valueRegister <- compileExpression env' value

        variable <- createVariable name
        putStatement $ Bitcast variable (llvmType $ getType value) valueRegister (llvmType $ getType value)
        let env' = insertVariable name variable env

        compileExpression env' body

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

    boxedRegister <- reserveRegister
    putStatement $ Load boxedRegister BoxedType positionRegister

    -- Tuple members are all boxed to ease generics on tuple members
    unbox boxedRegister t

box :: Operand -> LLVM.Type -> State CompilationState Operand
box sourceRegister realType = do
    -- Do not box if the type is already boxed
    if realType == BoxedType then return sourceRegister
    else do
        register <- reserveRegister

        -- Pointers have to be converted, other values are extended
        putStatement $ Comment "Boxing variable"
        case realType of
            PointerType -> putStatement $ PointerToInt register sourceRegister
            _ -> putStatement $ ZeroExtend register BoxedType sourceRegister realType

        return register

unbox :: Operand -> LLVM.Type -> State CompilationState Operand
unbox sourceRegister realType =
    -- Do not unbox if the type is interpreted as a polymorphic type
    if realType == BoxedType then return sourceRegister
    else do
        register <- reserveRegister

        -- Pointers have to be converted, other values are truncated
        putStatement $ Comment "Unboxing variable"
        case realType of
            PointerType -> putStatement $ IntToPointer register sourceRegister
            _ -> putStatement $ Truncate register realType sourceRegister BoxedType

        return register


-- State and environment utils

data CompilationState = CompilationState
    { register :: Int -- the last register to have been used
    , branch :: Int -- a counter for the number of branches in this expression
    , basicBlock :: Label -- the current basic block
    , variableCounter :: Map String Int -- used to provide a fresh variable name for shadowing
    , statements :: [Statement]
    }

initialState :: CompilationState
initialState = CompilationState 0 0 (MakeLabel "entry") empty []

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

    let leftLabel = MakeLabel $ "_left_" ++ show branchCount
        rightLabel = MakeLabel $ "_right_" ++ show branchCount
        mergeLabel = MakeLabel $ "_merge_" ++ show branchCount

    return (leftLabel, rightLabel, mergeLabel)

putLabel :: Label -> State CompilationState ()
putLabel label = do
    state <- get
    put state { basicBlock = label }
    putStatement $ Label label

getBasicBlock :: State CompilationState Label
getBasicBlock = do
    state <- get
    return $ basicBlock state

createVariable :: String -> State CompilationState Operand
createVariable name = do
    state <- get
    let counter = variableCounter state
    let variableCount = findWithDefault 0 name counter + 1
    put state { variableCounter = insert name variableCount counter }
    return $ variableOperand (name ++ "_" ++ show variableCount)

putStatement :: Statement -> State CompilationState ()
putStatement s = do
    state <- get
    put state { statements = statements state ++ [s] }

data CompilationEnvironment = CompilationEnvironment
    { environmentType :: LLVM.Type -- the type of the function closure environment
    , variableOperands :: Map String Operand
    -- When compiling a recursive function, we need to refer to the function's
    -- closure before the function has been assigned to the final local
    -- variable. As such, whenever there is a let the innermostLet variable is
    -- set, and it will be used when a closure is created to point to the
    -- already existing register rather than the final local variable
    , innermostLet :: Maybe String
    }

initialEnvironment :: CompilationEnvironment
initialEnvironment = CompilationEnvironment LLVM.VoidType empty Nothing

initialEnvironmentWithType :: LLVM.Type -> CompilationEnvironment
initialEnvironmentWithType t = CompilationEnvironment t empty Nothing

insertVariable :: String -> Operand -> CompilationEnvironment -> CompilationEnvironment
insertVariable s o env = env { variableOperands = insert s o operands }
    where operands = variableOperands env

insertLet :: String -> CompilationEnvironment -> CompilationEnvironment
insertLet s env = env { innermostLet = Just s }

-- Type utils

-- Converts a type to its LLVM representation. Tuples in the high-level code are
-- represented as pointers in the LLVM IR.
llvmType :: Closures.Type -> LLVM.Type
llvmType Closures.IntegerType = LLVM.IntegerType
llvmType Closures.BooleanType = LLVM.BooleanType
llvmType Closures.CharacterType = LLVM.CharacterType
llvmType (Closures.TupleType _) = LLVM.PointerType
llvmType Closures.SumType = LLVM.PointerType
llvmType (Closures.ClosureType _ _) = LLVM.PointerType
llvmType Closures.GenericType = LLVM.BoxedType

-- Despite the fact that variables holding tuples are represented with pointers
-- in the LLVM IR, to allocate them and acces their members we need to use their
-- tuple representation as an argument to the get element pointer instruction.
-- Additionally, all tuple values are stored in boxed form to facilitate access
-- when there are generic types. This function provides the corresponding
-- representation.
llvmTupleType :: Closures.Type -> LLVM.Type
llvmTupleType (Closures.TupleType ts) = LLVM.TupleType (map (\_ -> BoxedType) ts)
llvmTupleType _ = error "llvmTupleType should only be called on tuple types"

closureType :: LLVM.Type
closureType = LLVM.TupleType [LLVM.PointerType, LLVM.PointerType]

sumType :: LLVM.Type
sumType = LLVM.TupleType [LLVM.IntegerType, LLVM.PointerType]
