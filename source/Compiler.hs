{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Compiler (Compiler.compile) where

import Parser
import TypeChecker
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

    let enclosedProgram = encloseProgram typedProgram
    let llvm = Backend.compileProgram enclosedProgram

    writeFile output (show llvm)

    (code, _, stderr) <- readProcessWithExitCode "clang" ["-x" ,"ir", "-", "-o", output] (show llvm)
    case code of
        ExitFailure _ -> print stderr
        ExitSuccess -> return ()
