module Main where

import Errors
import Parser (parseProgram)
import TypeChecker

main :: IO ()
main = do
    let path = "test.btl"
    content <- readFile path

    case parseProgram content of
        Left e  -> showParseError path content e
        Right program -> case typeCheckProgram program of
            Left e  -> showTypeError path content e
            Right _ -> putStrLn "successful"
