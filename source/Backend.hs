{-# OPTIONS_GHC -Wincomplete-patterns #-}
{- HLINT ignore "Use <$>" -}

module Backend where

import LLVM
import Closures
import Control.Monad.State
import qualified Data.Map
import Data.Maybe (mapMaybe)

compile :: Closures.Program -> LLVM.Program
compile (Closures.Program definitions expression) = LLVM.Program (
        [ TargetTriple
        , FormatString
        , PrintfDeclaration
        , MallocDeclaration
        , TypeDeclaration (TypeVar "closure_type") (Tuple [Pointer, Pointer])
        ] ++
        mapMaybe compileEnvironment definitions ++
        (definitions >>= compileDefinition) ++
        [ LLVM.Function LLVM.Integer (GlobalVar "main") Nothing
            (compileMain expression)
        ]
    )

compileEnvironment :: FunctionDefinition -> Maybe TopLevelStatement
compileEnvironment (FunctionDefinition name argumentType returnType closure body) = Just closureDeclaration
    where closureDeclaration = TypeDeclaration (TypeVar (name ++ "_env")) (Tuple (map convertType closure))
compileEnvironment (BuiltInFunction _ _ _) = Nothing

compileDefinition :: FunctionDefinition -> [TopLevelStatement]
compileDefinition (FunctionDefinition name argumentType returnType closure body) = [definition]
    where closureDeclaration = TypeDeclaration (TypeVar (name ++ "_env")) (Tuple (map convertType closure))
          definition = LLVM.Function (convertType returnType) (GlobalVar name) (Just (convertType argumentType, ArgumentVar)) (expressionStatements ++ [returnStatement])
          (expressionStatements, compilationState) = runState (compileExpression Data.Map.empty body) (initialFunctionState (TypeVar (name ++ "_env")))
          returnStatement = Return (convertType returnType) (LocalOperand (RegisterVar (register compilationState)))
compileDefinition (BuiltInFunction _ _ _) = []

compileMain :: Expression -> [Statement]
compileMain expression = evalState (compileMainMonad expression) (initialFunctionState (TypeVar "_"))

compileMainMonad :: Expression -> State CompilationState [Statement]
compileMainMonad expression = do
    expressionStatements <- compileExpression Data.Map.empty expression
    register <- currentRegister
    return
        ( expressionStatements ++
            [ FormatStringPointer
            , PrintfCall (convertType (typeOf expression)) (RegisterVar register)
            , Return LLVM.Integer (Literal 0)
            ]
        )

compileExpression :: CurrentLetClosures -> Expression -> State CompilationState [Statement]
compileExpression clc (Closures.Integer value) = do
    register <- reserveRegister
    return [LocalAssign LLVM.Integer register Add (Literal 0) (Literal value)]
compileExpression clc (Closures.Boolean value) = do
    register <- reserveRegister
    let literalValue = if value then 1 else 0
    return [LocalAssign LLVM.Boolean register Add (Literal 0) (Literal literalValue)]
compileExpression clc (Argument t) = do
    register <- reserveRegister
    return [BitCast (LocalOperand (RegisterVar register)) (convertType t) (LocalOperand ArgumentVar) (convertType t)]
compileExpression clc (Variable index t) = do
    register <- reserveRegister
    let (Register registerNumber) = register
    let pointerVar = LocalOperand (LocalVar ("ptr_" ++ show registerNumber))
    envType <- currentEnvironmentType
    return
        [ GetElementPointer pointerVar envType (LocalOperand (LocalVar "env")) (Literal 0) (Literal index)
        , Load (convertType t) (LocalOperand (RegisterVar register)) pointerVar
        ]
compileExpression clc (Local name t) = do
    register <- reserveRegister

    let source = case Data.Map.lookup name clc of
            Just registerVar -> registerVar
            Nothing -> LocalVar name

    return [BitCast (LocalOperand (RegisterVar register)) (convertType t) (LocalOperand source) (convertType t)]
compileExpression clc ifExpression@(If condition thenBranch elseBranch) = do
    conditionContent <- compileExpression clc condition
    conditionRegister <- currentRegister

    -- To ensure that every label is unique within the function declaration,
    -- we append the register number to it
    let (Register n) = conditionRegister
    let thenLabel = Label ("then" ++ show n)
    let elseLabel = Label ("else" ++ show n)
    let mergeLabel = Label ("merge" ++ show n)

    setContext thenLabel
    thenContent <- compileExpression clc thenBranch
    thenRegister <- currentRegister
    thenContext <- currentContext

    setContext elseLabel
    elseContent <- compileExpression clc elseBranch
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
compileExpression clc (Closure function arguments) = do
    sizePointerRegister <- reserveRegister
    sizeRegister <- reserveRegister

    environmentRegister <- reserveRegister
    let (FunctionDefinition name argumentType _ _ _) = function

    size2PointerRegister <- reserveRegister
    size2Register <- reserveRegister

    closureRegister <- reserveRegister
    let (Register closureNumber) = closureRegister

    wL <- getWaitingLet
    let newClc = case wL of
            Just name -> Data.Map.insert name (RegisterVar closureRegister) clc
            Nothing -> error "waiting let should be defined"
    environmentFilling <- mapM (addToClosure newClc (TypeVar (name ++ "_env")) (LocalOperand (RegisterVar environmentRegister))) arguments

    finalRegister <- reserveRegister

    return (
            [ GetElementPointer2 (LocalOperand (RegisterVar sizePointerRegister)) (TypeVar (name ++ "_env")) NullPtr (Literal 1)
            , PtrToInt (LocalOperand (RegisterVar sizeRegister)) (LocalOperand (RegisterVar sizePointerRegister))
            , Malloc (LocalOperand (RegisterVar environmentRegister)) (LocalOperand (RegisterVar sizeRegister))
            , GetElementPointer2 (LocalOperand (RegisterVar size2PointerRegister)) (TypeVar ("closure_type")) NullPtr (Literal 1)
            , PtrToInt (LocalOperand (RegisterVar size2Register)) (LocalOperand (RegisterVar size2PointerRegister))
            , Malloc (LocalOperand (RegisterVar closureRegister)) (LocalOperand (RegisterVar size2Register))
            , GetElementPointer (LocalOperand (LocalVar ("fn_" ++ (show closureNumber)))) (TypeVar ("closure_type")) (LocalOperand (RegisterVar closureRegister)) (Literal 0) (Literal 0)
            , Store Pointer (GlobalOperand (GlobalVar name)) (LocalOperand (LocalVar ("fn_" ++ (show closureNumber))))
            , GetElementPointer (LocalOperand (LocalVar ("env_" ++ (show closureNumber)))) (TypeVar ("closure_type")) (LocalOperand (RegisterVar closureRegister)) (Literal 0) (Literal 1)
            , Store Pointer (LocalOperand (RegisterVar environmentRegister)) (LocalOperand (LocalVar ("env_" ++ (show closureNumber))))
            ] ++ (concat environmentFilling) ++
            [ BitCast (LocalOperand (RegisterVar finalRegister)) Pointer (LocalOperand (RegisterVar closureRegister)) Pointer
            ]
        )
compileExpression clc (Application (Application (Closure (BuiltInFunction "+" _ _) []) left) right) =
    compileBinaryOperation clc Add left right
compileExpression clc (Application (Application (Closure (BuiltInFunction "-" _ _) []) left) right) =
    compileBinaryOperation clc Sub left right
compileExpression clc (Application (Application (Closure (BuiltInFunction "==" _ _) []) left) right) =
    compileBinaryOperation clc Eq left right
compileExpression clc (Application (Application (Closure (BuiltInFunction "<" _ _) []) left) right) =
    compileBinaryOperation clc Slt left right
compileExpression clc (Application (Application (Closure (BuiltInFunction ">" _ _) []) left) right) =
    compileBinaryOperation clc Sgt left right
compileExpression clc (Application (Application (Closure (BuiltInFunction "<=" _ _) []) left) right) =
    compileBinaryOperation clc Sle left right
compileExpression clc (Application (Application (Closure (BuiltInFunction ">=" _ _) []) left) right) =
    compileBinaryOperation clc Sge left right

compileExpression clc application@(Application closure expression) = do
    let (ClosureType argumentType returnType) = typeOf closure
    closureStatements <- compileExpression clc closure
    closureRegister <- currentRegister

    expressionStatements <- compileExpression clc expression
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
compileExpression clc (Let name value expression) = do
    setWaitingLet name
    valueStatements <- compileExpression clc value
    valueRegister <- currentRegister

    expressionStatements <- compileExpression clc expression

    let t = convertType (typeOf value)
    return ( valueStatements ++
            [BitCast (LocalOperand (LocalVar name)) t (LocalOperand (RegisterVar valueRegister)) t] ++
            expressionStatements
        )

compileBinaryOperation :: CurrentLetClosures -> Operation -> Expression -> Expression -> State CompilationState [Statement]
compileBinaryOperation clc operation left right = do
    leftStatements <- compileExpression clc left
    leftRegister <- currentRegister
    rightStatements <- compileExpression clc right
    rightRegister <- currentRegister

    register <- reserveRegister
    return (leftStatements ++ rightStatements ++
            [ LocalAssign LLVM.Integer register operation
                (LocalOperand (RegisterVar leftRegister))
                (LocalOperand (RegisterVar rightRegister))
            ]
        )

addToClosure :: CurrentLetClosures -> TypeVar -> Operand -> Expression -> State CompilationState [Statement]
addToClosure clc environmentType environment expression = do
    calculation <- compileExpression clc expression
    valueRegister <- currentRegister
    register <- reserveRegister

    return (calculation ++
            [ GetElementPointer (LocalOperand (RegisterVar register)) environmentType environment (Literal 0) (Literal 0)
            , Store (convertType (typeOf expression)) (LocalOperand (RegisterVar valueRegister)) (LocalOperand (RegisterVar register))
            ]
        )

data CompilationState = CompilationState
    { context :: Label
    , register :: Register
    , environmentType :: TypeVar
    , waitingLet :: Maybe String
    }

type CurrentLetClosures = Data.Map.Map String LocalVar

initialFunctionState :: TypeVar -> CompilationState
initialFunctionState environmentType = CompilationState (Label "entry") (Register 0) environmentType Nothing

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

setWaitingLet :: String -> State CompilationState ()
setWaitingLet newWaitingLet = modify (\s -> s { waitingLet = Just newWaitingLet })

getWaitingLet :: State CompilationState (Maybe String)
getWaitingLet = do
    state <- get
    let wL = waitingLet state
    modify (\s -> s { waitingLet = Nothing })
    return wL
