import AST
import Parser
import TypeChecker
import Closures
import Backend
import Data.Map (empty)
import Data.Either (rights)
import Control.Monad.State (execState)
import System.Environment
import Control.Monad (unless, when)
import Distribution.Compat.Prelude (exitFailure)
import System.Process (callProcess, readProcessWithExitCode)
import Data.Text (splitOn)
import System.FilePath (dropExtension)
import System.Exit (ExitCode(..))

main :: IO ()
main = do
    --arguments <- getArgs
    --unless (length arguments == 1) $ do
    --    putStrLn "usage: beetle [filename]"
    --    exitFailure
--
    --let [filename] = arguments
    --content <- readFile filename
--
    --let program = case parseProgram content of
    --        Left message -> error (show message)
    --        Right program -> program
--
    --let typedProgram = case typeCheck program of
    --        Left message -> error message
    --        Right typedProgram -> typedProgram

    let llvm = compile exampleProgram

    print llvm

    --(code, _, stderr) <- readProcessWithExitCode "clang" ["-x" ,"ir", "-", "-o", dropExtension filename] (show llvm)
    --case code of
    --    ExitFailure _ -> print stderr
    --    ExitSuccess -> return ()
