module LLVM where

import AST
import TypeChecker
import Control.Monad.State
import Data.List

export :: TypedProgram -> String
export (assignments, expression) = inBoilerplate functions result n
    where functions = map exportAssignment assignments
          (result, _, n) = evalState (exportExpression expression "") 1

exportAssignment :: TypedAssignment -> String
exportAssignment (TypedAssignment symbol (TFunction argumentName argumentType body returnType)) = content
    where (Symbol nameString) = symbol
          (Symbol argumentString) = argumentName
          argumentTypeString = convertType argumentType
          returnTypeString = convertType returnType
          (innerExpression, _, n) = evalState (exportExpression body "") 1
          content = intercalate "\n" (
                [ "define " ++ returnTypeString ++ " @" ++ nameString ++ "(" ++ argumentTypeString ++ " %" ++ argumentString ++ ") {"
                ] ++ [innerExpression] ++
                [ "    ret " ++ returnTypeString ++ " %" ++ show n
                , "}"
                ]
            )
exportAssignment (TypedAssignment symbol expression) = do
    error "not implemented"

exportExpression :: TypedExpression -> String -> State Int (String, String, Int)
exportExpression (TInteger value) context = do
    n <- get
    let content = "    %" ++ show n ++ " = add i32 0, " ++ show value
    put (n + 1)
    return (content, context, n)
-- Hardcoded "+" implementation
exportExpression (TIf condition thenBranch elseBranch) context = do
    (conditionContent, _, conditionRegister) <- exportExpression condition context
    (thenContent, thenContext, thenRegister) <- exportExpression thenBranch context
    (elseContent, elseContext, elseRegister) <- exportExpression elseBranch context


    n <- get

    let thenContextFinal = if thenContext == "" then "true" ++ show n else thenContext
    let elseContextFinal = if elseContext == "" then "false" ++ show n else elseContext

    let content = intercalate "\n"
            [ conditionContent
            , "    br i1 %" ++ show conditionRegister ++ ", label %true" ++ show n ++ ", label %false" ++ show n
            , "true" ++ show n ++ ":"
            , thenContent
            , "    br label %merge" ++ show n
            , "false" ++ show n ++ ":"
            , elseContent
            , "    br label %merge" ++ show n
            , "merge" ++ show n ++ ":"
            , "    %" ++ show n ++ " = phi i32 [%" ++ show thenRegister ++ ", %" ++ thenContextFinal ++ "], [%" ++ show elseRegister ++ ", %" ++ elseContextFinal ++ "]"
            ]
    put (n + 1)

    return (content, "merge" ++ show n, n)
exportExpression (TApplication (TApplication (TVariable (Symbol "+") _) left) right) context = do
    (aContent, _, aRegister) <- exportExpression left context
    (bContent, _, bRegister) <- exportExpression right context

    n <- get
    let content = "    %" ++ show n ++ " = add i32 %" ++ show aRegister ++ ", %" ++ show bRegister
    put (n + 1)
    return (intercalate "\n" [aContent, bContent, content], context, n)
-- Hardcoded "-" implementation
exportExpression (TApplication (TApplication (TVariable (Symbol "-") _) left) right) context = do
    (aContent, _, aRegister) <- exportExpression left context
    (bContent, _, bRegister) <- exportExpression right context

    n <- get
    let content = "    %" ++ show n ++ " = sub i32 %" ++ show aRegister ++ ", %" ++ show bRegister
    put (n + 1)
    return (intercalate "\n" [aContent, bContent, content], context, n)
-- Hardcoded "==" implementation
exportExpression (TApplication (TApplication (TVariable (Symbol "==") _) left) right) context = do
    (aContent, _, aRegister) <- exportExpression left context
    (bContent, _, bRegister) <- exportExpression right context

    n <- get
    let content = "    %" ++ show n ++ " = icmp eq i32 %" ++ show aRegister ++ ", %" ++ show bRegister
    put (n + 1)
    return (intercalate "\n" [aContent, bContent, content], context, n)
exportExpression (TVariable (Symbol name) _) context = do
    n <- get
    let content = "    %" ++ show n ++ " = add i32 0, %" ++ name
    put (n + 1)
    return (content, context, n)
exportExpression (TApplication (TVariable (Symbol function) _) expression) context = do
    (expressionContent, _, expressionN) <- exportExpression expression context
    n <- get
    let content = intercalate "\n"
            [ expressionContent
            , "    %" ++ show n ++ " = call i32 @" ++ function ++ "(i32 %" ++ show expressionN ++ ")"
            ]
    put (n + 1)
    return (content, context, n)

inBoilerplate :: [String] -> String -> Int -> String
inBoilerplate functions content n = unlines (
        [ "target triple = \"x86_64-pc-linux-gnu\""
        , "@fmt = private constant [4 x i8] c\"%d\\0A\\00\""
        , "declare i32 @printf(i8*, ...)"
        , ""
        ] ++ functions ++
        [ ""
        , "define i32 @compute() {"
        , content
        , "    ret i32 %" ++ show n
        , "}"
        , ""
        , "define i32 @main() {"
        , "    %r = call i32 @compute()"
        , "    %fmt = getelementptr [4 x i8], [4 x i8]* @fmt, i64 0, i64 0"
        , "    call i32 (i8*, ...) @printf(i8* %fmt, i32 %r)"
        , "    ret i32 0"
        , "}"
        ]
    )

convertType :: Type -> String
convertType BooleanType = "i1"
convertType IntegerType = "i32"
