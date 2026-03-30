module Evaluator (execute) where

import AST
import Types
import Data.Map
import Control.Monad.State
import Prelude hiding (lookup)

type Environment = Map Symbol TypedExpression

execute :: TypedProgram -> TypedExpression
execute (assignments, expression) = evaluate environment expression
    where environment = execState (executeAux assignments) empty

executeAux :: [TypedAssignment] -> State Environment ()
executeAux [] = return ()
executeAux (assignment:program) = do
    environment <- get
    let (TypedAssignment symbol expression) = assignment
    assign assignment
    executeAux program

assign :: TypedAssignment -> State Environment ()
assign (TypedAssignment variable expression) = do
    environment <- get
    let value = evaluate environment expression
    let result = insert variable value environment
    put result

evaluate :: Environment -> TypedExpression -> TypedExpression
evaluate env (TBoolean b) = TBoolean b
evaluate env (TInteger n) = TInteger n
evaluate env (TVariable s t) = case lookup s env of
    Just n -> n
    Nothing -> error ("variable " ++ show s ++ "was not declared")
evaluate env (TIf condition a b) = case evaluate env condition of
    TBoolean True -> evaluate env a
    TBoolean False -> evaluate env b
    _ -> error "only boolean expressions can be used in if statements"
-- Hardcoded "+" implementation
evaluate env (TApplication (TApplication (TVariable (Symbol "+") _) a) b) = case evaluate env a of
    (TInteger x) -> case evaluate env b of
        (TInteger y) -> TInteger (x + y)
        _ -> error "could not evaluate sum right hand-side"
    _ -> error "could not evaluate sum left hand-side"
-- Hardcoded "+" implementation
evaluate env (TApplication (TApplication (TVariable (Symbol "-") _) a) b) = case evaluate env a of
    (TInteger x) -> case evaluate env b of
        (TInteger y) -> TInteger (x - y)
        _ -> error "could not evaluate subtraction right hand-side"
    _ -> error "could not evaluate subtraction left hand-side"
-- Hardcoded "==" implementation
evaluate env (TApplication (TApplication (TVariable (Symbol "==") _) a) b) = case evaluate env a of
    (TInteger x) -> case evaluate env b of
        (TInteger y) -> TBoolean (x == y)
        _ -> error "could not evaluate equality right hand-side or evaluated to wrong type"
    (TBoolean x) -> case evaluate env b of
        (TBoolean y) -> TBoolean (x == y)
        _ -> error "could not evaluate equality right hand-side or evaluated to wrong type"
    _ -> error "could not evaluate equality left hand-side"
evaluate env (TApplication (TVariable name _) expression) = case lookup name env of
    Just definition -> case definition of
        (TFunction input _ body) ->
            let inputValue = evaluate env expression
                newEnv = insert input inputValue env
            in evaluate newEnv body
        _ -> error ("variable " ++ show name ++ "is not a function")
    Nothing -> error ("variable " ++ show name ++ "was not declared")
evaluate env (TApplication _ _) = error "can only perform applications on functions"
evaluate env (TFunction input output t) = TFunction input output t
