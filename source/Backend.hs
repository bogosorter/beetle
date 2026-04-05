{-# OPTIONS_GHC -Wincomplete-patterns #-}
{- HLINT ignore "Use <$>" -}

module Backend where

import AST
import LLVM
import TypeChecker
import Control.Monad.State

compile :: TypedProgram -> LLVM.Program
compile (assignments, expression) = compiledProgram
    where declarations = map compileDeclaration assignments
          mainBody = compileMain assignments expression
          compiledProgram = LLVM.Program (
                [ TargetTriple
                , FormatString
                , PrintfDeclaration
                ] ++
                declarations ++
                [ LLVM.Function LLVM.Integer (GlobalVar "main") Nothing
                    mainBody
                ]
            )

-- These are top-level declarations of functions and global variables. Global
-- variable calculations cannot be done in the global scope, though, which means
-- that they can only be declared here, and initialization occurs inside the
-- main function (see compileMain).
compileDeclaration :: TypedAssignment -> TopLevelStatement
compileDeclaration (TypedAssignment (Symbol name) (TFunction (Symbol argument) argumentType body returnType)) = declaration
    where (expressionStatements, compilationState) = runState (compileExpression body) (initialFunctionState argument)
          returnStatement = Return (convertType returnType) (LocalOperand (RegisterVar (register compilationState)))
          declaration = LLVM.Function (convertType returnType) (GlobalVar name) (Just (convertType argumentType, ArgumentVar argument)) (expressionStatements ++ [returnStatement])
compileDeclaration (TypedAssignment (Symbol name) expression) =
    GlobalVariableDeclaration (convertType (typeOf expression)) (GlobalVar name)

compileMain :: [TypedAssignment] -> TypedExpression -> [Statement]
compileMain assignments expression = evalState (compileMainMonad assignments expression) initialState

data CompilationState = CompilationState
    { context :: Label
    , register :: Register
    , functionArgument :: Maybe String
    }

compileMainMonad :: [TypedAssignment] -> TypedExpression -> State CompilationState [Statement]
compileMainMonad [] expression = do
    expressionStatements <- compileExpression expression
    register <- currentRegister
    return
        ( expressionStatements ++
            [ FormatStringPointer
            , PrintfCall (convertType (typeOf expression)) (RegisterVar register)
            , Return LLVM.Integer (Literal 0)
            ]
        )
compileMainMonad (assignment:assignments) expression = do
    assignmentStatements <- compileAssignment assignment
    remaining <- compileMainMonad assignments expression
    return (assignmentStatements ++ remaining)

compileAssignment :: TypedAssignment -> State CompilationState [Statement]
compileAssignment (TypedAssignment (Symbol name) (TFunction {})) = return []
compileAssignment (TypedAssignment (Symbol name) expression) = do
    expressionStatements <- compileExpression expression
    register <- currentRegister
    return
        ( expressionStatements ++
          [Store (convertType (typeOf expression)) register (GlobalVar name)]
        )


compileExpression :: TypedExpression -> State CompilationState [Statement]

compileExpression (TInteger value) = do
    register <- reserveRegister
    return [LocalAssign LLVM.Integer register Add (Literal 0) (Literal value)]

compileExpression (TBoolean value) = do
    register <- reserveRegister
    let literalValue = if value then 1 else 0
    return [LocalAssign LLVM.Boolean register Add (Literal 0) (Literal literalValue)]

compileExpression (TVariable (Symbol name) _) = do
    register <- reserveRegister

    state <- get
    let result = if Just name == functionArgument state
        then [LocalAssign LLVM.Integer register Add (Literal 0) (LocalOperand (ArgumentVar name))]
        else [Load LLVM.Integer register (GlobalVar name)]

    return result

compileExpression (TIf condition thenBranch elseBranch) = do
    conditionContent <- compileExpression condition
    conditionRegister <- currentRegister

    -- To ensure that every label is unique within the function declaration,
    -- we append the register number to it
    let (Register n) = conditionRegister
    let thenLabel = Label ("then" ++ show n)
    let elseLabel = Label ("else" ++ show n)
    let mergeLabel = Label ("merge" ++ show n)

    setContext thenLabel
    thenContent <- compileExpression thenBranch
    thenRegister <- currentRegister
    thenContext <- currentContext

    setContext elseLabel
    elseContent <- compileExpression elseBranch
    elseRegister <- currentRegister
    elseContext <- currentContext

    setContext mergeLabel
    register <- reserveRegister
    let jumpStatement = Branch (RegisterVar conditionRegister) thenLabel elseLabel
    let mergeStatement = Phi register (convertType (typeOf thenBranch))
            (LocalOperand (RegisterVar thenRegister)) thenContext
            (LocalOperand (RegisterVar elseRegister)) elseContext

    return (
            conditionContent ++
            [jumpStatement] ++
            [LabelStatement thenLabel] ++
            thenContent ++
            [Jump mergeLabel] ++
            [LabelStatement elseLabel] ++
            elseContent ++
            [Jump mergeLabel] ++
            [LabelStatement mergeLabel] ++
            [mergeStatement]
        )

compileExpression (TFunction {}) = error "function expression should never be compiled"

compileExpression (TApplication (TApplication (TVariable (Symbol "+") _) left) right) =
    compileBinaryOperation Add left right
compileExpression (TApplication (TApplication (TVariable (Symbol "-") _) left) right) =
    compileBinaryOperation Sub left right
compileExpression (TApplication (TApplication (TVariable (Symbol "==") _) left) right) =
    compileBinaryOperation Eq left right

compileExpression (TApplication (TVariable (Symbol function) (FunctionType _ returnType)) expression) = do
    expressionStatements <- compileExpression expression
    expressionRegister <- currentRegister
    let expressionType = convertType (typeOf expression)

    register <- reserveRegister
    let functionLLVMType = convertType returnType
    let callStatement = Call expressionType register (GlobalVar function) expressionType (RegisterVar expressionRegister)
    return (expressionStatements ++ [callStatement])

compileExpression app@(TApplication {}) = do
    error "only direct applications are supported for now"

compileBinaryOperation :: Operation -> TypedExpression -> TypedExpression -> State CompilationState [Statement]
compileBinaryOperation operation left right = do
    leftStatements <- compileExpression left
    leftRegister <- currentRegister
    rightStatements <- compileExpression right
    rightRegister <- currentRegister

    register <- reserveRegister
    return (leftStatements ++ rightStatements ++
            [LocalAssign LLVM.Integer register operation
                (LocalOperand (RegisterVar leftRegister))
                (LocalOperand (RegisterVar rightRegister))
            ]
        )


-- Utilities that manipulate the compilation state

initialState :: CompilationState
initialState = CompilationState (Label "entry") (Register 0) Nothing

initialFunctionState :: String -> CompilationState
initialFunctionState argument = CompilationState (Label "entry") (Register 0) (Just argument)

currentRegister :: State CompilationState Register
currentRegister = gets register

incrementRegister :: State CompilationState ()
incrementRegister = do
    (Register n) <- currentRegister
    modify (\s -> s { register = Register (n + 1) })

reserveRegister :: State CompilationState Register
reserveRegister = do
    incrementRegister
    currentRegister

currentContext :: State CompilationState Label
currentContext = gets context

setContext :: Label -> State CompilationState ()
setContext newContext = modify (\s -> s { context = newContext })
