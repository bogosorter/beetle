{-# OPTIONS_GHC -Wincomplete-patterns #-}
{- HLINT ignore "Use <$>" -}

module Backend where

import LLVM
import Closures
import Control.Monad.State
import qualified Data.Map
import Data.Maybe (mapMaybe)
import Data.Foldable (foldrM, foldlM)

compile :: Closures.Program -> LLVM.Program
compile (Closures.Program definitions expression) = LLVM.Program (
        [ TargetTriple
        , FormatString
        , PrintfDeclaration
        , MallocDeclaration
        , TypeDeclaration (TypeVar "closure_type") (LLVM.Tuple [Pointer, Pointer])
        ] ++
        mapMaybe compileEnvironment definitions ++
        (definitions >>= compileDefinition) ++
        [ LLVM.Function LLVM.Integer (GlobalVar "main") Nothing
            (compileMain expression)
        ]
    )

compileEnvironment :: FunctionDefinition -> Maybe TopLevelStatement
compileEnvironment (FunctionDefinition name argumentType returnType closure body) = Just closureDeclaration
    where closureDeclaration = TypeDeclaration (TypeVar (name ++ "_env")) (LLVM.Tuple (map convertType closure))
compileEnvironment (BuiltInFunction _ _ _) = Nothing

compileDefinition :: FunctionDefinition -> [TopLevelStatement]
compileDefinition (FunctionDefinition name argumentType returnType closure body) = [definition]
    where closureDeclaration = TypeDeclaration (TypeVar (name ++ "_env")) (LLVM.Tuple (map convertType closure))
          definition = LLVM.Function (convertType returnType) (GlobalVar name) (Just (convertType argumentType, ArgumentVar)) (expressionStatements ++ [returnStatement])
          (expressionStatements, compilationState) = runState (compileExpression Data.Map.empty Nothing body) (initialFunctionState (TypeVar (name ++ "_env")))
          returnStatement = Return (convertType returnType) (LocalOperand (RegisterVar (register compilationState)))
compileDefinition (BuiltInFunction _ _ _) = []

compileMain :: Expression -> [Statement]
compileMain expression = evalState (compileMainMonad expression) (initialFunctionState (TypeVar "_"))

compileMainMonad :: Expression -> State CompilationState [Statement]
compileMainMonad expression = do
    expressionStatements <- compileExpression Data.Map.empty Nothing expression
    register <- currentRegister
    return
        ( expressionStatements ++
            [ FormatStringPointer
            , PrintfCall (convertType (typeOf expression)) (RegisterVar register)
            , Return LLVM.Integer (Literal 0)
            ]
        )

compileExpression :: CurrentLetClosures -> Maybe String -> Expression -> State CompilationState [Statement]
compileExpression clc waitingLets (Closures.Integer value) = do
    register <- reserveRegister
    return [LocalAssign LLVM.Integer register Add (Literal 0) (Literal value)]
compileExpression clc waitingLets (Closures.Boolean value) = do
    register <- reserveRegister
    let literalValue = if value then 1 else 0
    return [LocalAssign LLVM.Boolean register Add (Literal 0) (Literal literalValue)]
compileExpression clc waitingLets (Closures.Tuple expressions t) = do

    let compileTupleMember (statements, registers) member = do
            memberStatements <- compileExpression clc waitingLets member
            register <- currentRegister
            return (statements ++ memberStatements, registers ++ [register])

    (statements, registers) <- foldlM compileTupleMember ([], []) expressions

    sizePointerRegister <- reserveRegister
    sizeRegister <- reserveRegister
    resultRegister <- reserveRegister

    let storeTupleMember :: (Int, Register, LLVM.Type) -> [Statement] -> State CompilationState [Statement]
        storeTupleMember (index, member, memberType) statements = do
            storePointer <- reserveRegister
            let memberStatements =
                    [ GetElementPointer (LocalOperand (RegisterVar storePointer)) (LiteralType (convertToTupleType t)) (LocalOperand (RegisterVar resultRegister)) (Literal 0) (Literal index)
                    , Store memberType (LocalOperand (RegisterVar member)) (LocalOperand (RegisterVar storePointer))
                    ]
            return (statements ++ memberStatements)

    tupleStoreStatements <- foldrM storeTupleMember [] (zip3 [0..] registers (map (convertType . typeOf) expressions))

    lastRegister <- reserveRegister

    return
        ( statements ++
          [ GetElementPointer2 (LocalOperand (RegisterVar sizePointerRegister)) (LiteralType (convertToTupleType t)) NullPtr (Literal 1)
          , PtrToInt (LocalOperand (RegisterVar sizeRegister)) (LocalOperand (RegisterVar sizePointerRegister))
          , Malloc (LocalOperand (RegisterVar resultRegister)) (LocalOperand (RegisterVar sizeRegister))
          ] ++
          tupleStoreStatements ++
          -- This is a little trick to make the last register be that of the
          -- tuple. There should be a better way to do this, though
          [ BitCast (LocalOperand (RegisterVar lastRegister)) Pointer (LocalOperand (RegisterVar resultRegister)) Pointer
          ]
        )
compileExpression clc waitingLets (Argument t) = do
    register <- reserveRegister
    return [BitCast (LocalOperand (RegisterVar register)) (convertType t) (LocalOperand ArgumentVar) (convertType t)]
compileExpression clc waitingLets (Variable index t) = do
    register <- reserveRegister
    let (Register registerNumber) = register
    let pointerVar = LocalOperand (LocalVar ("ptr_" ++ show registerNumber))
    envType <- currentEnvironmentType
    return
        [ GetElementPointer pointerVar envType (LocalOperand (LocalVar "env")) (Literal 0) (Literal index)
        , Load (convertType t) (LocalOperand (RegisterVar register)) pointerVar
        ]

compileExpression clc waitingLets (Local name t) = do
    register <- reserveRegister

    let source = case Data.Map.lookup name clc of
            Just registerVar -> registerVar
            Nothing -> LocalVar name

    return [BitCast (LocalOperand (RegisterVar register)) (convertType t) (LocalOperand source) (convertType t)]

compileExpression clc waitingLets (TupleMember index base t) = do
    baseContent <- compileExpression clc waitingLets base
    baseRegister <- currentRegister

    addressRegister <- reserveRegister
    valueRegister <- reserveRegister

    return $
        baseContent ++
        [ GetElementPointer (LocalOperand (RegisterVar addressRegister)) (LiteralType (convertToTupleType (typeOf base))) (LocalOperand (RegisterVar baseRegister)) (Literal 0) (Literal index)
        , Load (convertType t) (LocalOperand (RegisterVar valueRegister)) (LocalOperand (RegisterVar addressRegister))
        ]

compileExpression clc waitingLets ifExpression@(If condition thenBranch elseBranch) = do
    conditionContent <- compileExpression clc waitingLets condition
    conditionRegister <- currentRegister

    -- To ensure that every label is unique within the function declaration,
    -- we append the register number to it
    let (Register n) = conditionRegister
    let thenLabel = Label ("then" ++ show n)
    let elseLabel = Label ("else" ++ show n)
    let mergeLabel = Label ("merge" ++ show n)

    setContext thenLabel
    thenContent <- compileExpression clc waitingLets thenBranch
    thenRegister <- currentRegister
    thenContext <- currentContext

    setContext elseLabel
    elseContent <- compileExpression clc waitingLets elseBranch
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
compileExpression clc waitingLets (Closure function arguments) = do
    sizePointerRegister <- reserveRegister
    sizeRegister <- reserveRegister

    environmentRegister <- reserveRegister
    let (FunctionDefinition name argumentType _ _ _) = function

    size2PointerRegister <- reserveRegister
    size2Register <- reserveRegister

    closureRegister <- reserveRegister
    let (Register closureNumber) = closureRegister
    let newClc = case waitingLets of
            Just waitingLet -> Data.Map.insert waitingLet (RegisterVar closureRegister) clc
            Nothing -> clc
    environmentFilling <- mapM (\(i, x) -> addToClosure i newClc waitingLets (TypeVar (name ++ "_env")) (LocalOperand (RegisterVar environmentRegister)) x) (zip [0..] arguments)

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
compileExpression clc waitingLets (Application (Application (Closure (BuiltInFunction "+" _ _) []) left) right) =
    compileBinaryOperation clc waitingLets Add left right
compileExpression clc waitingLets (Application (Application (Closure (BuiltInFunction "-" _ _) []) left) right) =
    compileBinaryOperation clc waitingLets Sub left right
compileExpression clc waitingLets (Application (Application (Closure (BuiltInFunction "==" _ _) []) left) right) =
    compileBinaryOperation clc waitingLets Eq left right
compileExpression clc waitingLets (Application (Application (Closure (BuiltInFunction "<" _ _) []) left) right) =
    compileBinaryOperation clc waitingLets Slt left right
compileExpression clc waitingLets (Application (Application (Closure (BuiltInFunction ">" _ _) []) left) right) =
    compileBinaryOperation clc waitingLets Sgt left right
compileExpression clc waitingLets (Application (Application (Closure (BuiltInFunction "<=" _ _) []) left) right) =
    compileBinaryOperation clc waitingLets Sle left right
compileExpression clc waitingLets (Application (Application (Closure (BuiltInFunction ">=" _ _) []) left) right) =
    compileBinaryOperation clc waitingLets Sge left right

compileExpression clc waitingLets application@(Application closure expression) = do
    let (ClosureType argumentType returnType) = typeOf closure
    closureStatements <- compileExpression clc waitingLets closure
    closureRegister <- currentRegister

    expressionStatements <- compileExpression clc waitingLets expression
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
compileExpression clc waitingLets (Let name value expression) = do
    valueStatements <- compileExpression clc (Just name) value
    valueRegister <- currentRegister

    expressionStatements <- compileExpression clc waitingLets expression

    let t = convertType (typeOf value)
    return ( valueStatements ++
            [BitCast (LocalOperand (LocalVar name)) t (LocalOperand (RegisterVar valueRegister)) t] ++
            expressionStatements
        )
compileExpression clc waitingLets (Closures.TupleDestructuring names value ensuing) = do
    valueStatements <- compileExpression clc Nothing value
    valueRegister <- currentRegister

    let destructureMember (name, t, i) = do
            addressRegister <- reserveRegister
            return
                [ GetElementPointer (LocalOperand (RegisterVar addressRegister)) (LiteralType (convertToTupleType (typeOf value))) (LocalOperand (RegisterVar valueRegister)) (Literal 0) (Literal i)
                , Load t (LocalOperand (LocalVar name)) (LocalOperand (RegisterVar addressRegister))
                ]

    let (LLVM.Tuple memberTypes) = convertToTupleType (typeOf value)
    destructureStatements <- mapM destructureMember (zip3 names memberTypes [0..])

    ensuingStatements <- compileExpression clc waitingLets ensuing

    return ( valueStatements ++
             concat destructureStatements ++
             ensuingStatements
        )

compileBinaryOperation :: CurrentLetClosures -> Maybe String -> Operation -> Expression -> Expression -> State CompilationState [Statement]
compileBinaryOperation clc waitingLets operation left right = do
    leftStatements <- compileExpression clc waitingLets left
    leftRegister <- currentRegister
    rightStatements <- compileExpression clc waitingLets right
    rightRegister <- currentRegister

    register <- reserveRegister
    return (leftStatements ++ rightStatements ++
            [ LocalAssign LLVM.Integer register operation
                (LocalOperand (RegisterVar leftRegister))
                (LocalOperand (RegisterVar rightRegister))
            ]
        )

addToClosure :: Int -> CurrentLetClosures -> Maybe String -> TypeVar -> Operand -> Expression -> State CompilationState [Statement]
addToClosure index clc waitingLets environmentType environment expression = do
    calculation <- compileExpression clc waitingLets expression
    valueRegister <- currentRegister
    register <- reserveRegister

    return (calculation ++
            [ GetElementPointer (LocalOperand (RegisterVar register)) environmentType environment (Literal 0) (Literal index)
            , Store (convertType (typeOf expression)) (LocalOperand (RegisterVar valueRegister)) (LocalOperand (RegisterVar register))
            ]
        )

data CompilationState = CompilationState
    { context :: Label
    , register :: Register
    , environmentType :: TypeVar
    , waitingLet :: [String]
    }

type CurrentLetClosures = Data.Map.Map String LocalVar

initialFunctionState :: TypeVar -> CompilationState
initialFunctionState environmentType = CompilationState (Label "entry") (Register 0) environmentType []

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
setWaitingLet newWaitingLet = modify (\s -> s { waitingLet = newWaitingLet : waitingLet s })

getWaitingLet :: State CompilationState String
getWaitingLet = do
    state <- get
    let (wL:remaining) = waitingLet state
    modify (\s -> s { waitingLet = remaining })
    return wL
