{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Backend (compileProgram) where

import Closures
import LLVM

import Control.Monad.State (State, runState, get, put)

compileProgram :: Closures.Program -> LLVM.Program
compileProgram program = LLVM.Program [] (s, e, llvmType $ getType mainExpression)
    where mainExpression = Closures.main program
          (e, CompilationState _ s) = runState (compileExpression mainExpression) initialState

-- Returns the statements required to obtain the expression and the register
-- where its value is stored
compileExpression :: Expression -> State CompilationState Operand
compileExpression expression = case expression of
    Integer n -> do
        register <- reserveRegister
        addStatement $ integerLiteral register n
        return register

    Boolean b -> do
        register <- reserveRegister
        addStatement $ booleanLiteral register b
        return register

    Local name _ -> do
        return $ variableOperand name

    -- Built-in function calls
    Application (Application (BuiltInFunction name _) left _) right _ -> do
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

        addStatement $ Operation register (llvmType $ getType expression) opCode leftRegister rightRegister
        return register

    Let name value body t -> do
        valueRegister <- compileExpression value

        let variable = variableOperand name
        addStatement $ Bitcast variable (llvmType t) valueRegister (llvmType t)

        compileExpression body

    _ -> error "not implemented"

integerLiteral :: Operand -> Int -> Statement
integerLiteral operand n = Operation operand LLVM.IntegerType Add (integerOperand 0) (integerOperand n)

booleanLiteral :: Operand -> Bool -> Statement
booleanLiteral operand n = Operation operand LLVM.BooleanType Add (integerOperand 0) (booleanOperand n)
    where booleanOperand False = integerOperand 0
          booleanOperand True = integerOperand 1


-- State utils

data CompilationState = CompilationState
    { register :: Int
    , statements :: [Statement]
    }

initialState :: CompilationState
initialState = CompilationState 0 []

reserveRegister :: State CompilationState Operand
reserveRegister = do
    state <- get
    let registerNumber = register state + 1
    put state { register = registerNumber }
    return $ registerOperand registerNumber

addStatement :: Statement -> State CompilationState ()
addStatement s = do
    state <- get
    put state { statements = statements state ++ [s] }


-- Type utils

llvmType :: Closures.Type -> LLVM.Type
llvmType Closures.IntegerType = LLVM.IntegerType
llvmType Closures.BooleanType = LLVM.BooleanType
