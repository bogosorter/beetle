{-# OPTIONS_GHC -Wincomplete-patterns #-}

import AST
import Parser
import TypeChecker
import Closures
import Enclose
import Backend
import Data.Map (empty)
import Data.Either (rights)
import Control.Monad.State (execState)
import System.Environment
import Control.Monad (unless, when)
import Distribution.Compat.Prelude (exitFailure, exitSuccess)
import System.Process (callProcess, readProcessWithExitCode)
import Data.Text (splitOn)
import System.FilePath (dropExtension)
import System.Exit (ExitCode(..))

main :: IO ()
main = do
    arguments <- getArgs
    parsedArguments <- case parseArguments arguments of
        Just result -> return result
        Nothing -> do
            putStrLn "usage: beetle <filename>"
            exitFailure

    content <- readFile (inputFile parsedArguments)

    let program = case parseProgram content of
            Left message -> error (show message)
            Right program -> program

    let typedProgram = case typeCheck program of
            Left message -> error message
            Right typedProgram -> typedProgram

    unless (outputType parsedArguments /= AST) $ do
        writeFile (outputFile parsedArguments) (show program)
        exitSuccess

    let enclosedProgram = enclose typedProgram

    unless (outputType parsedArguments /= IR) $ do
        writeFile (outputFile parsedArguments) (show enclosedProgram)
        exitSuccess

    let llvm = compile enclosedProgram

    unless (outputType parsedArguments /= LLVM) $ do
        writeFile (outputFile parsedArguments) (show llvm)
        exitSuccess

    (code, _, stderr) <- readProcessWithExitCode "clang" ["-x" ,"ir", "-", "-o", outputFile parsedArguments] (show llvm)
    case code of
        ExitFailure _ -> print stderr
        ExitSuccess -> return ()


data Arguments = Arguments
    { inputFile :: String
    , outputFile :: String
    , outputType :: OutputType
    }

data OutputType = AST | IR | LLVM | Binary deriving Eq

parseArguments :: [String] -> Maybe Arguments
parseArguments arguments = case parseInputFile arguments of
    Just inputFile -> let outputType = parseOutputType arguments
        in Just Arguments
            { inputFile = inputFile
            , outputFile = parseOutputFile arguments inputFile outputType
            , outputType = outputType
            }
    Nothing -> Nothing

-- The input file is assumed to be the first string that is not associated with
-- a flag
parseInputFile :: [String] -> Maybe String
parseInputFile [] = Nothing
parseInputFile [a]
    | take 1 a == "-" = Nothing
    | otherwise = Just a
parseInputFile (a:b:cs)
    -- -o flag requires a filename, which means that we skip two arguments
    | a == "-o" = parseInputFile cs
    | take 1 a == "-" = parseInputFile (b:cs)
    | otherwise = Just a

parseOutputType :: [String] -> OutputType
parseOutputType [] = Binary
parseOutputType (argument:arguments)
    | argument == "-ast" = AST
    | argument == "-ir" = IR
    | argument == "-ll" = LLVM
    | otherwise = parseOutputType arguments

parseOutputFile :: [String] -> String -> OutputType -> String
parseOutputFile [] inputFile outputType = dropExtension inputFile ++ outputTypeExtension outputType
parseOutputFile [_] inputFile outputType = dropExtension inputFile ++ outputTypeExtension outputType
parseOutputFile (flag:filename:rest) inputFile outputType
    | flag == "-o" = filename
    | otherwise = parseOutputFile (filename : rest) inputFile outputType

outputTypeExtension :: OutputType -> String
outputTypeExtension AST = ".ast"
outputTypeExtension IR = ".ir"
outputTypeExtension LLVM = ".ll"
outputTypeExtension Binary = ""
