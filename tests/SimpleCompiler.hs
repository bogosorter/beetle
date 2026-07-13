module SimpleCompiler (compile, compileWithTypeErrors) where

import Parser
import TypeChecker
import ASTSimplificator
import Encloser
import Backend

import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

compile :: String -> String -> IO ()
compile input output = do
    content <- readFile input

    let program = case parseProgram content of
            Left message -> error (show message)
            Right program -> program

    let typedProgram = case typeCheckProgram program of
            Left message -> error (show message)
            Right typedProgram -> typedProgram


    let simplifiedProram = simplify typedProgram
        enclosedProgram = encloseProgram simplifiedProram
        llvm = Backend.compileProgram enclosedProgram

    (code, _, stderr) <- readProcessWithExitCode "clang" ["-x" ,"ir", "-", "-o", output] (show llvm)
    case code of
        ExitFailure _ -> print stderr
        ExitSuccess -> return ()

compileWithTypeErrors :: String -> IO (Maybe ())
compileWithTypeErrors input = do
    content <- readFile input

    let program = case parseProgram content of
            Left message -> error (show message)
            Right program -> program

    case typeCheckProgram program of
        Left _ -> return $ Just ()
        Right _ -> return Nothing
