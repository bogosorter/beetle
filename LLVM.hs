module LLVM (export) where

import AST
import TypeChecker
import Control.Monad.State
import Data.List

export :: TypedProgram -> String
export program@(assignments, expression) = inBoilerplate declarations main lastRegister
    where declarations = map exportAssignmentDeclaration assignments
          (main, lastRegister) = exportMain program

exportAssignmentDeclaration :: TypedAssignment -> String
exportAssignmentDeclaration (TypedAssignment symbol (TFunction argumentName argumentType body returnType)) = content
    where (Symbol nameString) = symbol
          (Symbol argumentString) = argumentName
          argumentTypeString = convertType argumentType
          returnTypeString = convertType returnType
          (innerExpression, _, n) = evalState (exportExpression body "" (Just argumentString)) 1
          content = intercalate "\n" (
                [ "define " ++ returnTypeString ++ " @" ++ nameString ++ "(" ++ argumentTypeString ++ " %" ++ argumentString ++ ") {"
                ] ++ [innerExpression] ++
                [ "    ret " ++ returnTypeString ++ " %" ++ show n
                , "}"
                ]
            )
exportAssignmentDeclaration (TypedAssignment (Symbol name) expression) = content
    where content = "@" ++ name ++ " = global " ++ convertType variableType ++ " 0"
          variableType = typeOf expression

exportMain :: TypedProgram -> (String, Int)
exportMain program = (main, lastRegister)
    where (main, lastRegister) = evalState (exportMainMonad program) 1

exportMainMonad :: TypedProgram -> State Int (String, Int)
exportMainMonad ([], expression) = do
    (main, _, lastRegister) <- exportExpression expression "" Nothing
    return (main, lastRegister)
exportMainMonad (assignment:assignments, expression) = do
    content <- assignmentExpression assignment
    (subContent, lastRegister) <- exportMainMonad (assignments, expression)
    return (content ++ "\n" ++ subContent, lastRegister)

assignmentExpression :: TypedAssignment -> State Int String
-- Functions have no need of putting code in main
assignmentExpression (TypedAssignment symbol (TFunction {})) = return ""
assignmentExpression (TypedAssignment (Symbol name) expression) = do
    (expressionContent, _, lastRegister) <- exportExpression expression "" Nothing
    return (expressionContent ++ "\n" ++ "    store " ++ convertType (typeOf expression) ++ " %" ++ show lastRegister ++ ", ptr @" ++ name)

exportExpression :: TypedExpression -> String -> Maybe String -> State Int (String, String, Int)
exportExpression (TInteger value) context _ = do
    n <- get
    let content = "    %" ++ show n ++ " = add i32 0, " ++ show value
    put (n + 1)
    return (content, context, n)
-- Hardcoded "+" implementation
exportExpression (TIf condition thenBranch elseBranch) context argument = do
    (conditionContent, _, conditionRegister) <- exportExpression condition context argument
    (thenContent, thenContext, thenRegister) <- exportExpression thenBranch context argument
    (elseContent, elseContext, elseRegister) <- exportExpression elseBranch context argument


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
exportExpression (TApplication (TApplication (TVariable (Symbol "+") _) left) right) context argument = do
    (aContent, _, aRegister) <- exportExpression left context argument
    (bContent, _, bRegister) <- exportExpression right context argument

    n <- get
    let content = "    %" ++ show n ++ " = add i32 %" ++ show aRegister ++ ", %" ++ show bRegister
    put (n + 1)
    return (intercalate "\n" [aContent, bContent, content], context, n)
-- Hardcoded "-" implementation
exportExpression (TApplication (TApplication (TVariable (Symbol "-") _) left) right) context argument = do
    (aContent, _, aRegister) <- exportExpression left context argument
    (bContent, _, bRegister) <- exportExpression right context argument

    n <- get
    let content = "    %" ++ show n ++ " = sub i32 %" ++ show aRegister ++ ", %" ++ show bRegister
    put (n + 1)
    return (intercalate "\n" [aContent, bContent, content], context, n)
-- Hardcoded "==" implementation
exportExpression (TApplication (TApplication (TVariable (Symbol "==") _) left) right) context argument = do
    (aContent, _, aRegister) <- exportExpression left context argument
    (bContent, _, bRegister) <- exportExpression right context argument

    n <- get
    let content = "    %" ++ show n ++ " = icmp eq i32 %" ++ show aRegister ++ ", %" ++ show bRegister
    put (n + 1)
    return (intercalate "\n" [aContent, bContent, content], context, n)
exportExpression (TVariable (Symbol name) _) context argument = do
    n <- get
    let varContent = "    %" ++ show n ++ " = load i32, ptr @" ++ name
    let argContent = "    %" ++ show n ++ " = add i32 0, %" ++ name
    let actualContent = case argument of
            Nothing -> varContent
            Just argName -> if name == argName then argContent else varContent
    put (n + 1)
    return (actualContent, context, n)
exportExpression (TApplication (TVariable (Symbol function) _) expression) context argument = do
    (expressionContent, _, expressionN) <- exportExpression expression context argument
    n <- get
    let content = intercalate "\n"
            [ expressionContent
            , "    %" ++ show n ++ " = call i32 @" ++ function ++ "(i32 %" ++ show expressionN ++ ")"
            ]
    put (n + 1)
    return (content, context, n)
exportExpression expression context argument = do
    error (show expression)

inBoilerplate :: [String] -> String -> Int -> String
inBoilerplate declarations content n = unlines (
        [ "target triple = \"x86_64-pc-linux-gnu\""
        , "@fmt = private constant [4 x i8] c\"%d\\0A\\00\""
        , "declare i32 @printf(i8*, ...)"
        , ""
        ] ++ intersperse "\n" declarations ++
        [ ""
        , "define i32 @main() {"
        , content
        , "    %fmt = getelementptr [4 x i8], [4 x i8]* @fmt, i64 0, i64 0"
        , "    call i32 (i8*, ...) @printf(i8* %fmt, i32 %" ++ show n ++ ")"
        , "    ret i32 0"
        , "}"
        ]
    )

convertType :: Type -> String
convertType BooleanType = "i1"
convertType IntegerType = "i32"
