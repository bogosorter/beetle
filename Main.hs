import AST
import Evaluator
import Parser
import Types
import Data.Map (empty)
import Data.Either (rights)
import Control.Monad.State (execState)

main :: IO ()
main = do
    content <- getContents

    let program = case parseProgram content of
            Left message -> error (show message)
            Right program -> program

    let typedProgram = case typeCheck program of
            Left message -> error message
            Right typedProgram -> typedProgram

    print (execute typedProgram)
