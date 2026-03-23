module Evaluator where

import AST
import Data.Map
import Control.Monad.State
import Prelude hiding (lookup)

execute :: [Assignment] -> State Environment ()
execute [] = return ()
execute (assignment:program) = do
    environment <- get
    let (Assignment symbol expression) = assignment
    assign assignment
    execute program

assign :: Assignment -> State Environment ()
assign (Assignment variable expression) = do
    environment <- get
    let value = evaluate environment expression
    let result = insert variable value environment
    put result

evaluate :: Environment -> Expression -> Expression
evaluate env (BooleanLiteral b) = BooleanLiteral b
evaluate env (IntegerLiteral n) = IntegerLiteral n
evaluate env (Variable s) = case lookup s env of
    Just n -> n
    Nothing -> error ("variable " ++ show s ++ "was not declared")
evaluate env (If condition a b) = case evaluate env condition of
    BooleanLiteral True -> evaluate env a
    BooleanLiteral False -> evaluate env b
    _ -> error "only boolean expressions can be used in if statements"
-- Hardcoded "+" implementation
evaluate env (Application (Application (Variable (Symbol "+")) a) b) = case evaluate env a of
    (IntegerLiteral x) -> case evaluate env b of
        (IntegerLiteral y) -> IntegerLiteral (x + y)
        _ -> error "could not evaluate sum right hand-side"
    _ -> error "could not evaluate sum left hand-side"
-- Hardcoded "+" implementation
evaluate env (Application (Application (Variable (Symbol "-")) a) b) = case evaluate env a of
    (IntegerLiteral x) -> case evaluate env b of
        (IntegerLiteral y) -> IntegerLiteral (x - y)
        _ -> error "could not evaluate subtraction right hand-side"
    _ -> error "could not evaluate subtraction left hand-side"
-- Hardcoded "==" implementation
evaluate env (Application (Application (Variable (Symbol "==")) a) b) = case evaluate env a of
    (IntegerLiteral x) -> case evaluate env b of
        (IntegerLiteral y) -> BooleanLiteral (x == y)
        _ -> error "could not evaluate equality right hand-side or evaluated to wrong type"
    (BooleanLiteral x) -> case evaluate env b of
        (BooleanLiteral y) -> BooleanLiteral (x == y)
        _ -> error "could not evaluate equality right hand-side or evaluated to wrong type"
    _ -> error "could not evaluate equality left hand-side"
evaluate env (Application (Variable name) expression) = case lookup name env of
    Just definition -> case definition of
        (Function input output) ->
            let inputValue = evaluate env expression
                newEnv = insert input inputValue env
            in evaluate newEnv output
        _ -> error ("variable " ++ show name ++ "is not a function")
    Nothing -> error ("variable " ++ show name ++ "was not declared")
evaluate env (Application _ _) = error "can only perform applications on functions"
evaluate env (Function input output) = Function input output
