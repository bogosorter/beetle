module Main where

import Arguments
import Errors

import Parser
import TypeChecker
import Encloser
import Backend

import System.Exit (ExitCode(..), exitFailure, exitSuccess)
import System.Process (readProcessWithExitCode)
import Control.Monad (when)


main :: IO ()
main = do
    arguments <- parseArguments
    parsedArguments <- case arguments of
        Just args -> return args
        Nothing -> do
            putStrLn "usage: beetle <filename>"
            exitFailure

    let path = inputFile parsedArguments
    content <- readFile path

    parsedProgram <- case parseProgram content of
        Right program -> return program
        Left e -> do
            showParseError path content e
            exitFailure
    checkpoint AST parsedArguments parsedProgram

    typedProgram <- case typeCheckProgram parsedProgram of
        Right program -> return program
        Left e -> do
            showTypeError path content e
            exitFailure
    checkpoint TypeChecked parsedArguments typedProgram

    let enclosedProgram = encloseProgram typedProgram
    checkpoint IR parsedArguments enclosedProgram

    let compiledProgram = compileProgram enclosedProgram
    checkpoint LLVM parsedArguments compiledProgram

    (code, _, stderr) <- readProcessWithExitCode "clang" ["-x" ,"ir", "-", "-o", outputFile parsedArguments] (show compiledProgram)
    case code of
        ExitFailure _ -> print stderr
        ExitSuccess -> return ()

-- Terminates compilation if the desired stage has already been reached
checkpoint :: Show a => OutputType -> Arguments -> a -> IO ()
checkpoint stage arguments value =
    when (stage == outputType arguments) $ do
        writeFile (outputFile arguments) (show value)
        exitSuccess
