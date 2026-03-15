import AST
import Evaluator
import Parser
import Data.Map (empty)
import Data.Either (rights)
import Control.Monad.State (execState)

main :: IO ()
main = do
    content <- getContents
    (assignments, expression) <- case parseProgram content of
        Left err -> error (show err)
        Right p -> return p

    let environment = execState (execute assignments) empty
    print (evaluate environment expression)
