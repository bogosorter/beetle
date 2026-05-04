{-# OPTIONS_GHC -Wincomplete-patterns #-}
{- HLINT ignore "Use <$>" -}

module Backend where

import LLVM
import Closures
import Control.Monad.State

compile :: Closures.Program -> LLVM.Program
compile (definitions, expression) = LLVM.Program (
        [ TargetTriple
        , FormatString
        , PrintfDeclaration
        , MallocDeclaration
        , TypeDeclaration (TypeVar "closure_type") (Tuple [Pointer, Pointer])
        ] ++
        (definitions >>= compileEnvironment) ++
        (definitions >>= compileDefinition) ++
        [ LLVM.Function LLVM.Integer (GlobalVar "main") Nothing
            (compileMain expression)
        ]
    )

compileEnvironment :: FunctionDefinition -> [TopLevelStatement]
compileEnvironment (FunctionDefinition name argumentType returnType closure body) = [closureDeclaration]
    where closureDeclaration = TypeDeclaration (TypeVar (name ++ "_env")) (Tuple (map convertType closure))
compileEnvironment (BuiltInFunction _) = []

compileDefinition :: FunctionDefinition -> [TopLevelStatement]
compileDefinition (FunctionDefinition name argumentType returnType closure body) = [definition]
    where closureDeclaration = TypeDeclaration (TypeVar (name ++ "_env")) (Tuple (map convertType closure))
          definition = LLVM.Function (convertType returnType) (GlobalVar name) (Just (convertType argumentType, ArgumentVar)) (expressionStatements ++ [returnStatement])
          (expressionStatements, compilationState) = runState (compileExpression body) (initialFunctionState (TypeVar (name ++ "_env")))
          returnStatement = Return (convertType returnType) (LocalOperand (RegisterVar (register compilationState)))
compileDefinition (BuiltInFunction _) = []

compileMain :: Expression -> [Statement]
compileMain expression = evalState (compileMainMonad expression) (initialFunctionState (TypeVar "_"))

compileMainMonad :: Expression -> State CompilationState [Statement]
compileMainMonad expression = do
    expressionStatements <- compileExpression expression
    register <- currentRegister
    return
        ( expressionStatements ++
            [ FormatStringPointer
            , PrintfCall (convertType (typeOf expression)) (RegisterVar register)
            , Return LLVM.Integer (Literal 0)
            ]
        )

compileExpression :: Expression -> State CompilationState [Statement]
compileExpression (Closures.Integer value) = do
    register <- reserveRegister
    return [LocalAssign LLVM.Integer register Add (Literal 0) (Literal value)]
compileExpression (Closures.Boolean value) = do
    register <- reserveRegister
    let literalValue = if value then 1 else 0
    return [LocalAssign LLVM.Boolean register Add (Literal 0) (Literal literalValue)]
compileExpression (Argument _) = do
    register <- reserveRegister
    return [LocalAssign LLVM.Integer register Add (Literal 0) (LocalOperand ArgumentVar)]
compileExpression (Variable index _) = do
    register <- reserveRegister
    let (Register registerNumber) = register
    let pointerVar = LocalOperand (LocalVar ("ptr_" ++ show registerNumber))
    envType <- currentEnvironmentType
    return
        [ GetElementPointer pointerVar envType (LocalOperand (LocalVar "env")) (Literal 0) (Literal index)
        , Load LLVM.Integer (LocalOperand (RegisterVar register)) pointerVar
        ]
compileExpression (Local name t) = do
    register <- reserveRegister
    return [BitCast (LocalOperand (RegisterVar register)) (convertType t) (LocalOperand (LocalVar name)) (convertType t)]
compileExpression ifExpression@(If condition thenBranch elseBranch) = do
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
    let mergeStatement = Phi register (convertType (typeOf ifExpression))
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
compileExpression (Closure function arguments) = do
    sizePointerRegister <- reserveRegister
    sizeRegister <- reserveRegister

    environmentRegister <- reserveRegister
    let (FunctionDefinition name argumentType _ _ _) = function
    environmentFilling <- mapM (addToClosure (TypeVar (name ++ "_env")) (LocalOperand (RegisterVar environmentRegister))) arguments

    size2PointerRegister <- reserveRegister
    size2Register <- reserveRegister

    closureRegister <- reserveRegister
    let (Register closureNumber) = closureRegister

    return (
            [ GetElementPointer2 (LocalOperand (RegisterVar sizePointerRegister)) (TypeVar (name ++ "_env")) NullPtr (Literal 1)
            , PtrToInt (LocalOperand (RegisterVar sizeRegister)) (LocalOperand (RegisterVar sizePointerRegister))
            , Malloc (LocalOperand (RegisterVar environmentRegister)) (LocalOperand (RegisterVar sizeRegister))
            ] ++ (concat environmentFilling) ++
            [ GetElementPointer2 (LocalOperand (RegisterVar size2PointerRegister)) (TypeVar ("closure_type")) NullPtr (Literal 1)
            , PtrToInt (LocalOperand (RegisterVar size2Register)) (LocalOperand (RegisterVar size2PointerRegister))
            , Malloc (LocalOperand (RegisterVar closureRegister)) (LocalOperand (RegisterVar size2Register))
            , GetElementPointer (LocalOperand (LocalVar ("fn_" ++ (show closureNumber)))) (TypeVar ("closure_type")) (LocalOperand (RegisterVar closureRegister)) (Literal 0) (Literal 0)
            , Store Pointer (GlobalOperand (GlobalVar name)) (LocalOperand (LocalVar ("fn_" ++ (show closureNumber))))
            , GetElementPointer (LocalOperand (LocalVar ("env_" ++ (show closureNumber)))) (TypeVar ("closure_type")) (LocalOperand (RegisterVar closureRegister)) (Literal 0) (Literal 1)
            , Store Pointer (LocalOperand (RegisterVar environmentRegister)) (LocalOperand (LocalVar ("env_" ++ (show closureNumber))))
            ]
        )
compileExpression (Application (Application (Closure (BuiltInFunction "+") []) left) right) =
    compileBinaryOperation Add left right
compileExpression (Application (Application (Closure (BuiltInFunction "-") []) left) right) =
    compileBinaryOperation Sub left right
compileExpression (Application (Application (Closure (BuiltInFunction "==") []) left) right) =
    compileBinaryOperation Eq left right
compileExpression (Application (Application (Closure (BuiltInFunction "<") []) left) right) =
    compileBinaryOperation Slt left right
compileExpression (Application (Application (Closure (BuiltInFunction ">") []) left) right) =
    compileBinaryOperation Sgt left right
compileExpression (Application (Application (Closure (BuiltInFunction "<=") []) left) right) =
    compileBinaryOperation Sle left right
compileExpression (Application (Application (Closure (BuiltInFunction ">=") []) left) right) =
    compileBinaryOperation Sge left right

compileExpression application@(Application closure expression) = do
    let (ClosureType argumentType returnType) = typeOf closure
    closureStatements <- compileExpression closure
    closureRegister <- currentRegister

    expressionStatements <- compileExpression expression
    expressionRegister <- currentRegister

    functionPointerRegister <- reserveRegister
    functionRegister <- reserveRegister
    environmentPointerRegister <- reserveRegister
    environmentRegister <- reserveRegister
    result <- reserveRegister

    return (closureStatements ++ expressionStatements ++
            [ GetElementPointer (LocalOperand (RegisterVar functionPointerRegister)) (TypeVar "closure_type") (LocalOperand (RegisterVar closureRegister)) (Literal 0) (Literal 0)
            , Load Pointer (LocalOperand (RegisterVar functionRegister)) (LocalOperand (RegisterVar functionPointerRegister))
            , GetElementPointer (LocalOperand (RegisterVar environmentPointerRegister)) (TypeVar "closure_type") (LocalOperand (RegisterVar closureRegister)) (Literal 0) (Literal 1)
            , Load Pointer (LocalOperand (RegisterVar environmentRegister)) (LocalOperand (RegisterVar environmentPointerRegister))
            , Call (convertType returnType) (LocalOperand (RegisterVar result)) (LocalOperand (RegisterVar functionRegister)) (LocalOperand (RegisterVar environmentRegister)) (convertType argumentType) (RegisterVar expressionRegister)
            ]
        )
compileExpression (Let name value expression) = do
    valueStatements <- compileExpression value
    valueRegister <- currentRegister

    expressionStatements <- compileExpression expression

    let t = convertType (typeOf value)
    return ( valueStatements ++
            [BitCast (LocalOperand (LocalVar name)) t (LocalOperand (RegisterVar valueRegister)) t] ++
            expressionStatements
        )

compileBinaryOperation :: Operation -> Expression -> Expression -> State CompilationState [Statement]
compileBinaryOperation operation left right = do
    leftStatements <- compileExpression left
    leftRegister <- currentRegister
    rightStatements <- compileExpression right
    rightRegister <- currentRegister

    register <- reserveRegister
    return (leftStatements ++ rightStatements ++
            [ LocalAssign LLVM.Integer register operation
                (LocalOperand (RegisterVar leftRegister))
                (LocalOperand (RegisterVar rightRegister))
            ]
        )

addToClosure :: TypeVar -> Operand -> Expression -> State CompilationState [Statement]
addToClosure environmentType environment expression = do
    calculation <- compileExpression expression
    valueRegister <- currentRegister
    register <- reserveRegister

    return (calculation ++
            [ GetElementPointer (LocalOperand (RegisterVar register)) environmentType environment (Literal 0) (Literal 0)
            , Store LLVM.Integer (LocalOperand (RegisterVar valueRegister)) (LocalOperand (RegisterVar register))
            ]
        )

data CompilationState = CompilationState
    { context :: Label
    , register :: Register
    , environmentType :: TypeVar
    }

initialFunctionState :: TypeVar -> CompilationState
initialFunctionState environmentType = CompilationState (Label "entry") (Register 0) environmentType

currentRegister :: State CompilationState Register
currentRegister = gets register

currentEnvironmentType :: State CompilationState TypeVar
currentEnvironmentType = gets environmentType

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

-- compile :: Closures.Program -> LLVM.Program
-- compile (assignments, expression) = compiledProgram
--     where declarations = map compileDeclaration assignments
--           mainBody = compileMain assignments expression
--           compiledProgram = LLVM.Program (
--                 [ TargetTriple
--                 , FormatString
--                 , PrintfDeclaration
--                 ] ++
--                 declarations ++
--                 [ LLVM.Function LLVM.Integer (GlobalVar "main") Nothing
--                     mainBody
--                 ]
--             )
--
-- type Closure = [(String, Type)]
--
-- -- These are top-level declarations of functions and global variables. Global
-- -- variable calculations cannot be done in the global scope, though, which means
-- -- that they can only be declared here, and initialization occurs inside the
-- -- main function (see compileMain).
-- compileDeclaration :: TypedAssignment -> [TopLevelStatement]
-- compileDeclaration (TypedAssignment (Symbol name) (TFunction (Symbol argument) argumentType body returnType)) =
--         [ envDeclaration
--         , declaration
--         ]
--     where (expressionStatements, compilationState) = runState (compileExpression body) (initialFunctionState argument)
--           returnStatement = Return (convertType returnType) (LocalOperand (RegisterVar (register compilationState)))
--           env = _
--           envDeclaration = TypeDeclaration (TypeVar (name ++ "_env")) (envType env)
--           declaration = LLVM.Function (convertType returnType) (GlobalVar name) (Just (convertType argumentType, ArgumentVar argument)) (expressionStatements ++ [returnStatement])
-- compileDeclaration (TypedAssignment (Symbol name) expression) =
--     [GlobalVariableDeclaration (convertType (typeOf expression)) (GlobalVar name)]
--
-- buildClosure :: TypedExpression -> Closure
--
--
-- compileMain :: [TypedAssignment] -> TypedExpression -> [Statement]
-- compileMain assignments expression = evalState (compileMainMonad assignments expression) initialState
--
-- data CompilationState = CompilationState
--     { context :: Label
--     , register :: Register
--     , functionArgument :: Maybe String
--     }
--
-- compileMainMonad :: [TypedAssignment] -> TypedExpression -> State CompilationState [Statement]
-- compileMainMonad [] expression = do
--     expressionStatements <- compileExpression expression
--     register <- currentRegister
--     return
--         ( expressionStatements ++
--             [ FormatStringPointer
--             , PrintfCall (convertType (typeOf expression)) (RegisterVar register)
--             , Return LLVM.Integer (Literal 0)
--             ]
--         )
-- compileMainMonad (assignment:assignments) expression = do
--     assignmentStatements <- compileAssignment assignment
--     remaining <- compileMainMonad assignments expression
--     return (assignmentStatements ++ remaining)
--
-- compileAssignment :: TypedAssignment -> State CompilationState [Statement]
-- compileAssignment (TypedAssignment (Symbol name) (TFunction {})) = return []
-- compileAssignment (TypedAssignment (Symbol name) expression) = do
--     expressionStatements <- compileExpression expression
--     register <- currentRegister
--     return
--         ( expressionStatements ++
--           [Store (convertType (typeOf expression)) register (GlobalVar name)]
--         )
--
--
-- compileExpression :: TypedExpression -> State CompilationState [Statement]
--
-- compileExpression (TInteger value) = do
--     register <- reserveRegister
--     return [LocalAssign LLVM.Integer register Add (Literal 0) (Literal value)]
--
-- compileExpression (TBoolean value) = do
--     register <- reserveRegister
--     let literalValue = if value then 1 else 0
--     return [LocalAssign LLVM.Boolean register Add (Literal 0) (Literal literalValue)]
--
-- compileExpression (TVariable (Symbol name) _) = do
--     register <- reserveRegister
--
--     state <- get
--     let result = if Just name == functionArgument state
--         then [LocalAssign LLVM.Integer register Add (Literal 0) (LocalOperand (ArgumentVar name))]
--         else [Load LLVM.Integer register (GlobalVar name)]
--
--     return result
--
-- compileExpression (TIf condition thenBranch elseBranch) = do
--     conditionContent <- compileExpression condition
--     conditionRegister <- currentRegister
--
--     -- To ensure that every label is unique within the function declaration,
--     -- we append the register number to it
--     let (Register n) = conditionRegister
--     let thenLabel = Label ("then" ++ show n)
--     let elseLabel = Label ("else" ++ show n)
--     let mergeLabel = Label ("merge" ++ show n)
--
--     setContext thenLabel
--     thenContent <- compileExpression thenBranch
--     thenRegister <- currentRegister
--     thenContext <- currentContext
--
--     setContext elseLabel
--     elseContent <- compileExpression elseBranch
--     elseRegister <- currentRegister
--     elseContext <- currentContext
--
--     setContext mergeLabel
--     register <- reserveRegister
--     let jumpStatement = Branch (RegisterVar conditionRegister) thenLabel elseLabel
--     let mergeStatement = Phi register (convertType (typeOf thenBranch))
--             (LocalOperand (RegisterVar thenRegister)) thenContext
--             (LocalOperand (RegisterVar elseRegister)) elseContext
--
--     return (
--             conditionContent ++
--             [jumpStatement] ++
--             [LabelStatement thenLabel] ++
--             thenContent ++
--             [Jump mergeLabel] ++
--             [LabelStatement elseLabel] ++
--             elseContent ++
--             [Jump mergeLabel] ++
--             [LabelStatement mergeLabel] ++
--             [mergeStatement]
--         )
--
-- compileExpression (TFunction {}) = error "function expression should never be compiled"
--
-- compileExpression (TApplication (TApplication (TVariable (Symbol "+") _) left) right) =
--     compileBinaryOperation Add left right
-- compileExpression (TApplication (TApplication (TVariable (Symbol "-") _) left) right) =
--     compileBinaryOperation Sub left right
-- compileExpression (TApplication (TApplication (TVariable (Symbol "==") _) left) right) =
--     compileBinaryOperation Eq left right
-- compileExpression (TApplication (TApplication (TVariable (Symbol "<") _) left) right) =
--     compileBinaryOperation Slt left right
-- compileExpression (TApplication (TApplication (TVariable (Symbol ">") _) left) right) =
--     compileBinaryOperation Sgt left right
-- compileExpression (TApplication (TApplication (TVariable (Symbol "<=") _) left) right) =
--     compileBinaryOperation Sle left right
-- compileExpression (TApplication (TApplication (TVariable (Symbol ">=") _) left) right) =
--     compileBinaryOperation Sge left right
--
-- compileExpression (TApplication (TVariable (Symbol function) (FunctionType _ returnType)) expression) = do
--     expressionStatements <- compileExpression expression
--     expressionRegister <- currentRegister
--     let expressionType = convertType (typeOf expression)
--
--     register <- reserveRegister
--     let functionLLVMType = convertType returnType
--     let callStatement = Call expressionType register (GlobalVar function) expressionType (RegisterVar expressionRegister)
--     return (expressionStatements ++ [callStatement])
--
-- compileExpression app@(TApplication {}) = do
--     error "only direct applications are supported for now"
--
-- compileBinaryOperation :: Operation -> TypedExpression -> TypedExpression -> State CompilationState [Statement]
-- compileBinaryOperation operation left right = do
--     leftStatements <- compileExpression left
--     leftRegister <- currentRegister
--     rightStatements <- compileExpression right
--     rightRegister <- currentRegister
--
--     register <- reserveRegister
--     return (leftStatements ++ rightStatements ++
--             [LocalAssign LLVM.Integer register operation
--                 (LocalOperand (RegisterVar leftRegister))
--                 (LocalOperand (RegisterVar rightRegister))
--             ]
--         )
--
--
-- -- Utilities that manipulate the compilation state
--
-- initialState :: CompilationState
-- initialState = CompilationState (Label "entry") (Register 0) Nothing
--
-- initialFunctionState :: String -> CompilationState
-- initialFunctionState argument = CompilationState (Label "entry") (Register 0) (Just argument)
--
-- currentRegister :: State CompilationState Register
-- currentRegister = gets register
--
-- incrementRegister :: State CompilationState ()
-- incrementRegister = do
--     (Register n) <- currentRegister
--     modify (\s -> s { register = Register (n + 1) })
--
-- reserveRegister :: State CompilationState Register
-- reserveRegister = do
--     incrementRegister
--     currentRegister
--
-- currentContext :: State CompilationState Label
-- currentContext = gets context
--
-- setContext :: Label -> State CompilationState ()
-- setContext newContext = modify (\s -> s { context = newContext })
--
