{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Backend (compileProgram) where

import Closures
import LLVM

import Control.Monad.State (State, evalState, get, put)

compileProgram :: Closures.Program -> LLVM.Program
compileProgram program = LLVM.Program [] (s, e, llvmType $ getType mainExpression)
    where mainExpression = Closures.main program
          (s, e) = evalState (compileExpression mainExpression) initialState

-- Returns the statements required to obtain the expression and the register
-- where its value is stored
compileExpression :: Expression -> State CompilationState ([Statement], Operand)
compileExpression expression = case expression of
    Integer n -> do
        register <- reserveRegister
        return ([integerLiteral register n], register)
    Boolean b -> do
        register <- reserveRegister
        return ([booleanLiteral register b], register)
    _ -> error "not implemented"

integerLiteral :: Operand -> Int -> Statement
integerLiteral operand n = Operation operand (typeOperand LLVM.IntegerType) Add (integerOperand 0) (integerOperand n)

booleanLiteral :: Operand -> Bool -> Statement
booleanLiteral operand n = Operation operand (typeOperand LLVM.BooleanType) Add (integerOperand 0) (booleanOperand n)
    where booleanOperand False = integerOperand 0
          booleanOperand True = integerOperand 1


-- State utils

data CompilationState = CompileState
    { register :: Int
    }

initialState :: CompilationState
initialState = CompileState 0

reserveRegister :: State CompilationState Operand
reserveRegister = do
    state <- get
    let registerNumber = register state + 1
    put state { register = registerNumber }
    return $ registerOperand registerNumber

-- Type utils

llvmType :: Closures.Type -> LLVM.Type
llvmType Closures.IntegerType = LLVM.IntegerType
llvmType Closures.BooleanType = LLVM.BooleanType
