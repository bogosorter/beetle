{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Backend (compileProgram) where

import Closures
import LLVM

import Control.Monad.State (State, runState, get, put)

compileProgram :: Closures.Program -> LLVM.Program
compileProgram program = LLVM.Program [] (statements s, e, llvmType $ getType mainExpression)
    where mainExpression = Closures.main program
          (e, s) = runState (compileExpression mainExpression) initialState

-- Returns the statements required to obtain the expression and the register
-- where its value is stored
compileExpression :: Expression -> State CompilationState Operand
compileExpression expression = case expression of
    Integer n -> do
        register <- reserveRegister
        putStatement $ integerLiteral register n
        return register

    Boolean b -> do
        register <- reserveRegister
        putStatement $ booleanLiteral register b
        return register

    Tuple members t -> do
        register <- allocate t

        let insertMember :: (Int, Closures.Expression) -> State CompilationState ()
            insertMember (index, member) = do
                let memberType = llvmType $ getType member
                memberRegister <- compileExpression member

                positionRegister <- reserveRegister
                putStatement $ GetElementPointer positionRegister (llvmTupleType t) register (integerOperand 0) (integerOperand index)
                putStatement $ Store memberRegister memberType positionRegister

        mapM_ insertMember (zip [0..] members)
        return register

    Local name _ -> do
        return $ variableOperand name

    TupleMember index tuple t -> do
        tupleRegister <- compileExpression tuple

        positionRegister <- reserveRegister
        putStatement $ GetElementPointer positionRegister (llvmTupleType $ getType tuple) tupleRegister (integerOperand 0) (integerOperand index)

        resultRegister <- reserveRegister
        putStatement $ Load resultRegister (llvmType t) positionRegister
        return resultRegister

    If condition left right t -> do
        (leftLabel, rightLabel, mergeLabel) <- createBranch

        conditionRegister <- compileExpression condition
        putStatement $ Branch conditionRegister leftLabel rightLabel

        putBasicBlock leftLabel
        putStatement $ Label leftLabel
        leftRegister <- compileExpression left
        leftLabel <- getBasicBlock
        putStatement $ Jump mergeLabel

        putBasicBlock rightLabel
        putStatement $ Label rightLabel
        rightRegister <- compileExpression right
        rightLabel <- getBasicBlock
        putStatement $ Jump mergeLabel

        putBasicBlock mergeLabel
        register <- reserveRegister
        putStatement $ Label mergeLabel
        putStatement $ Phi register (llvmType t) leftRegister leftLabel rightRegister rightLabel

        return register

    -- Built-in function calls
    Application (Application f@(BuiltInFunction name _) left _) right _ -> do
        leftRegister <- compileExpression left
        rightRegister <- compileExpression right

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
                ClosuredType argumentType _ -> argumentType
                _ -> error "found a built-in function whose type is not a closured type"

        putStatement $ BinaryOperation register (llvmType $ argumentType) opCode leftRegister rightRegister
        return register

    Let name value body _ -> do
        valueRegister <- compileExpression value

        let variable = variableOperand name
        putStatement $ Bitcast variable (llvmType $ getType value) valueRegister (llvmType $ getType value)

        compileExpression body

    _ -> error "not implemented"

allocate :: Closures.Type -> State CompilationState Operand
allocate t = do
    sizePointerRegister <- reserveRegister
    putStatement $ GetElementPointer sizePointerRegister (llvmTupleType t) LLVM.null (integerOperand 1) (integerOperand 0)

    sizeRegister <- reserveRegister
    putStatement $ PointerToInt sizeRegister sizePointerRegister

    resultRegister <- reserveRegister
    putStatement $ Malloc resultRegister sizeRegister

    return resultRegister

integerLiteral :: Operand -> Int -> Statement
integerLiteral operand n = BinaryOperation operand LLVM.IntegerType Add (integerOperand 0) (integerOperand n)

booleanLiteral :: Operand -> Bool -> Statement
booleanLiteral operand n = BinaryOperation operand LLVM.BooleanType Add (integerOperand 0) (booleanOperand n)
    where booleanOperand False = integerOperand 0
          booleanOperand True = integerOperand 1


-- State utils

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


-- Type utils

-- Converts a type to its LLVM representation. Tuples in the high-level code are
-- represented as pointers in the LLVM IR.
llvmType :: Closures.Type -> LLVM.Type
llvmType Closures.IntegerType = LLVM.IntegerType
llvmType Closures.BooleanType = LLVM.BooleanType
llvmType (Closures.TupleType _) = LLVM.PointerType

-- Despite the fact that variables holding tuples are represented with pointers
-- in the LLVM IR, to allocate them we need to use their tuple representation as
-- an argument to the get element pointer instruction. This function provides
-- the corresponding representation.
llvmTupleType :: Closures.Type -> LLVM.Type
llvmTupleType (Closures.TupleType ts) = LLVM.TupleType (map llvmType ts)
llvmTupleType _ = error "llvmTupleType should only be called on tuple types"
