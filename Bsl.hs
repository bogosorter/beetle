{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Data.Map
import Control.Monad.State
import Prelude hiding (lookup)

type Program = [Assignment]
type Environment = Map Symbol Expression

newtype Symbol = Symbol String deriving (Eq, Ord)
data Assignment = Assignment Symbol Expression
data Expression = Literal Int | Variable Symbol | Function Symbol Expression | Application Expression Expression
    deriving Show

instance Show Symbol where
    show (Symbol s) = s

-- > 3 + 1
program1 = []
expression1 = Application (Application (Variable (Symbol "+")) (Literal 3)) (Literal 1)
environment1 = execState (execute program1) empty
value1 = evaluate environment1 expression1

-- x = 3
-- > x + 3
program2 = [Assignment (Symbol "x") (Literal 3)]
expression2 = Application (Application (Variable (Symbol "+")) (Variable (Symbol "x"))) (Literal 3)
environment2 = execState (execute program2) empty
value2 = evaluate environment2 expression2

-- s(x): x + 1
-- > s(1)
program3 = [Assignment (Symbol "s") (Function (Symbol "x") (Application (Application (Variable (Symbol "+")) (Variable (Symbol "x"))) (Literal 1)))]
expression3 = Application (Variable (Symbol "s")) (Literal 1)
environment3 = execState (execute program3) empty
value3 = evaluate environment3 expression3


execute :: Program -> State Environment ()
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
evaluate env (Literal n) = Literal n
-- Hardcoded "+" implementation
evaluate env (Application (Application (Variable (Symbol "+")) a) b) = case evaluate env a of
    (Literal x) -> case evaluate env b of
        (Literal y) -> Literal (x + y)
        _ -> error "could not evaluate sum right hand-side"
    _ -> error "could not evaluate sum left hand-side"
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
evaluate env (Variable s) = case lookup s env of
    Just n -> n
    Nothing -> error ("variable " ++ show s ++ "was not declared")
