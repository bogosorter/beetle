{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Data.Map
import Control.Monad.State
import Prelude hiding (lookup)

type Program = [Assignment]

data Assignment = Assignment Variable Expression

newtype Variable = Variable String deriving (Eq, Ord)
instance Show Variable where
    show (Variable name) = name

data Expression
    = Literal Int
    | Addition Expression Expression
    | Subtraction Expression Expression
    | Multiplication Expression Expression
    | Division Expression Expression
    | Var Variable

type Environment = Map Variable Int

execute :: Program -> State Environment ()
execute [] = return ()
execute (assignment:program) = do
    environment <- get
    let (Assignment variable expression) = assignment
    assign assignment
    execute program

assign :: Assignment -> State Environment ()
assign (Assignment variable expression) = do
    environment <- get
    let value = evaluate environment expression
    let result = insert variable value environment
    put result

evaluate :: Environment -> Expression -> Int
evaluate env (Literal n) = n
evaluate env (Addition e1 e2) = evaluate env e1 + evaluate env e2
evaluate env (Subtraction e1 e2) = evaluate env e1 - evaluate env e2
evaluate env (Multiplication e1 e2) = evaluate env e1 * evaluate env e2
evaluate env (Division e1 e2) = evaluate env e1 `div` evaluate env e2
evaluate env (Var v) = case lookup v env of
    Just n -> n
    Nothing -> error ("variable " ++ show v ++ "was not declared")

-- > 3 + 1
program1 = []
expression1 = Addition (Literal 3) (Literal 1)
environment1 = execState (execute program1) empty
value1 = evaluate environment1 expression1

-- x = 3
-- > x + 3
program2 = [Assignment (Variable "x") (Literal 3)]
expression2 = Addition (Var (Variable "x")) (Literal 3)
environment2 = execState (execute program2) empty
value2 = evaluate environment2 expression2

-- s(x): x + 1
-- > s(1)
