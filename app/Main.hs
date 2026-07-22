{-# LANGUAGE TemplateHaskell #-}

module Main where

import Arguments
import Errors

import Parser
import TypeChecker
import ASTSimplificator
import Encloser
import Backend
import LLVM

import System.Exit (ExitCode(..), exitFailure, exitSuccess)
import System.Process (readProcessWithExitCode)
import System.Directory (findExecutable)
import System.IO.Temp (withSystemTempFile)
import System.IO (hClose)
import Data.ByteString (ByteString, hPut)
import Data.FileEmbed (embedFileRelative)
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
            putStrLn $ showParseError path content e
            exitFailure
    checkpoint AST parsedArguments parsedProgram

    typedProgram <- case typeCheckProgram parsedProgram of
        Right program -> return program
        Left e -> do
            putStrLn $ showTypeError path content e
            exitFailure
    checkpoint TypeChecked parsedArguments typedProgram

    let simplifiedProgram = simplify typedProgram
    checkpoint Simplified parsedArguments simplifiedProgram

    let enclosedProgram = encloseProgram simplifiedProgram
    checkpoint IR parsedArguments enclosedProgram

    let compiledProgram = compileProgram enclosedProgram
    checkpoint LLVM parsedArguments compiledProgram

    clang parsedArguments compiledProgram

-- Terminates compilation if the desired stage has already been reached
checkpoint :: Show a => OutputType -> Arguments -> a -> IO ()
checkpoint stage arguments value =
    when (stage == outputType arguments) $ do
        writeFile (outputFile arguments) (show value)
        exitSuccess

clang :: Arguments -> LLVM.Program -> IO ()
clang arguments program = do
    hasClang <- findExecutable "clang"
    when (hasClang == Nothing) $ do
        putStrLn "please install clang before compiling with beetle"
        exitFailure

    withSystemTempFile "runtime.o" $ \runtimePath runtimeHandle -> do
        hPut runtimeHandle runtime
        hClose runtimeHandle

        (code, _, stderr) <- readProcessWithExitCode "clang" ["-o", outputFile arguments, runtimePath, "-x" ,"ir", "-"] (show program)
        case code of
            ExitFailure _ -> print stderr
            ExitSuccess -> return ()

runtime :: ByteString
runtime = $(embedFileRelative "runtime/runtime.o")
