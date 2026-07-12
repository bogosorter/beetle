{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Arguments (Arguments(..), OutputType(..), parseArguments) where

import System.Environment (getArgs)
import System.FilePath (dropExtension)
import Control.Monad (guard)

data Arguments = Arguments
    { inputFile :: String
    , outputFile :: String
    , outputType :: OutputType
    }

data OutputType = AST | TypeChecked | IR | LLVM | Binary deriving Eq


parseArguments :: IO (Maybe Arguments)
parseArguments = do
    arguments <- getArgs
    let tokens = tokenize arguments
    return $ retrieveArguments tokens


data Token = InputFile String | OutputFile String | OutputTypeToken OutputType

tokenize :: [String] -> [Token]
tokenize [] = []
tokenize ("-o":f:rest) = OutputFile f : tokenize rest
tokenize ("-ast":rest) = OutputTypeToken AST : tokenize rest
tokenize ("-tc":rest) = OutputTypeToken TypeChecked : tokenize rest
tokenize ("-ir":rest) = OutputTypeToken IR : tokenize rest
tokenize ("-ll":rest) = OutputTypeToken LLVM : tokenize rest
tokenize (f:rest) = InputFile f : tokenize rest


retrieveArguments :: [Token] -> Maybe Arguments
retrieveArguments tokens = do
    -- retrieveArguments makes use of a bunch of helper methods that get an
    -- argument and return the list of tokens without it.
    (inputFile, tokens') <- retrieveInputFile tokens
    let (outputType, tokens'') = retrieveOutputType tokens'
    let (outputFile, tokens) = retrieveOutputFile inputFile outputType tokens''

    guard (length tokens == 0)
    return Arguments
        { inputFile = inputFile
        , outputFile = outputFile
        , outputType = outputType
        }

retrieveInputFile :: [Token] -> Maybe (String, [Token])
retrieveInputFile [] = Nothing
retrieveInputFile (InputFile f:tokens) = Just (f, tokens)
retrieveInputFile (t:tokens) = do
    (f, tokens') <- retrieveInputFile tokens
    return (f, t : tokens')

retrieveOutputType :: [Token] -> (OutputType, [Token])
retrieveOutputType [] = (Binary, [])
retrieveOutputType (OutputTypeToken t:tokens) = (t, tokens)
retrieveOutputType (t:tokens) = (outputType, t : tokens')
    where (outputType, tokens') = retrieveOutputType tokens

retrieveOutputFile :: String -> OutputType -> [Token] -> (String, [Token])
retrieveOutputFile inputFile outputType [] = (dropExtension inputFile ++ outputTypeExtension outputType, [])
retrieveOutputFile inputFile outputType (OutputFile f:tokens) = (f, tokens)
retrieveOutputFile inputFile outputType (t:tokens) = (outputFile, t : tokens')
    where (outputFile, tokens') = retrieveOutputFile inputFile outputType tokens

outputTypeExtension :: OutputType -> String
outputTypeExtension AST = ".ast"
outputTypeExtension TypeChecked = ".tc"
outputTypeExtension IR = ".ir"
outputTypeExtension LLVM = ".ll"
outputTypeExtension Binary = ""
