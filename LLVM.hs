module LLVM where

import AST
import TypeChecker
import Control.Monad.State
import Data.List

export :: TypedProgram -> String
export (assignments, expression) = inBoilerplate result n
    where (result, n) = evalState (exportExpression expression) 1

exportExpression :: TypedExpression -> State Int (String, Int)
exportExpression (TInteger value) = do
    n <- get
    let content = "    %" ++ show n ++ " = add i32 0, " ++ show value
    put (n + 1)
    return (content, n)
-- Hardcoded "+" implementation
exportExpression (TIf condition thenBranch elseBranch) = do
    (conditionContent, conditionRegister) <- exportExpression condition
    (thenContent, thenRegister) <- exportExpression thenBranch
    (elseContent, elseRegister) <- exportExpression elseBranch

    n <- get

    let content = unlines
            [ conditionContent
            , "    br i1 %" ++ show conditionRegister ++ ", label %true, label %false"
            , "true:"
            , thenContent
            , "    br label %merge"
            , "false:"
            , elseContent
            , "    br label %merge"
            , "merge:"
            , "    %" ++ show n ++ " = phi i32 [%" ++ show thenRegister ++ ", %true], [%" ++ show elseRegister ++ ", %false]"
            ]
    put (n + 1)

    return (content, n)
exportExpression (TApplication (TApplication (TVariable (Symbol "+") _) left) right) = do
    (aContent, aRegister) <- exportExpression left
    (bContent, bRegister) <- exportExpression right

    n <- get
    let content = "    %" ++ show n ++ " = add i32 %" ++ show aRegister ++ ", %" ++ show bRegister
    put (n + 1)
    return (intercalate "\n" [aContent, bContent, content], n)
-- Hardcoded "-" implementation
exportExpression (TApplication (TApplication (TVariable (Symbol "-") _) left) right) = do
    (aContent, aRegister) <- exportExpression left
    (bContent, bRegister) <- exportExpression right

    n <- get
    let content = "    %" ++ show n ++ " = sub i32 %" ++ show aRegister ++ ", %" ++ show bRegister
    put (n + 1)
    return (intercalate "\n" [aContent, bContent, content], n)
-- Hardcoded "==" implementation
exportExpression (TApplication (TApplication (TVariable (Symbol "==") _) left) right) = do
    (aContent, aRegister) <- exportExpression left
    (bContent, bRegister) <- exportExpression right

    n <- get
    let content = "    %" ++ show n ++ " = icmp eq i32 %" ++ show aRegister ++ ", %" ++ show bRegister
    put (n + 1)
    return (intercalate "\n" [aContent, bContent, content], n)

inBoilerplate :: String -> Int -> String
inBoilerplate content n = unlines
    [ "target triple = \"x86_64-pc-linux-gnu\""
    , "@fmt = private constant [4 x i8] c\"%d\\0A\\00\""
    , "declare i32 @printf(i8*, ...)"
    , "define i32 @compute() {"
    , content
    , "    ret i32 %" ++ show n
    , "}"
    , "define i32 @main() {"
    , "    %r = call i32 @compute()"
    , "    %fmt = getelementptr [4 x i8], [4 x i8]* @fmt, i64 0, i64 0"
    , "    call i32 (i8*, ...) @printf(i8* %fmt, i32 %r)"
    , "    ret i32 0"
    , "}"
    ]
