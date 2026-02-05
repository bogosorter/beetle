import AST
import Evaluator
import Parser
import Data.Map (empty)
import Control.Monad.State (execState)

{-
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
-}

main :: IO ()
main = do
    contents <- getContents
    let (program, expression) = case parser contents of
            Right (program, expression) -> (program, expression)
            Left err -> error  (show err)
    let environment = execState (execute program) empty
    print (evaluate environment expression)
