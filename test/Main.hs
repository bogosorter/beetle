module Main where

import Parser (parseProgram)
import TypeChecker (typeCheckProgram)

import Control.Monad.Extra (concatMapM)
import Data.List (isSuffixOf)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
    files <- getFiles "tests"

    parsingTests <- mapM testParsing files
    typeCheckingTests <- mapM testTypeChecking files

    defaultMain $ testGroup "all"
        [ testGroup "parse" parsingTests
        , testGroup "type-check" typeCheckingTests
        ]

testParsing :: FilePath -> IO TestTree
testParsing path = do
    content <- readFile path
    return $ testCase path $ case parseProgram content of
        Right _ -> return ()
        Left error -> assertFailure (errorBundlePretty error)

testTypeChecking :: FilePath -> IO TestTree
testTypeChecking path = do
    content <- readFile path
    let parsed = case parseProgram content of
            Right parsed -> parsed
            Left _ -> error "program should parse"
    return $ testCase path $ case typeCheckProgram parsed of
        Right _ -> return ()
        Left message -> assertFailure (show message)


-- Utils

getFiles :: FilePath -> IO [FilePath]
getFiles path = do
    allFiles <- walk path
    return $ filter (".btl" `isSuffixOf`) allFiles

walk :: FilePath -> IO [FilePath]
walk path = do
    isDirectory <- doesDirectoryExist path
    case isDirectory of
        False -> return [path]
        True -> walkDirectory path

walkDirectory :: FilePath -> IO [FilePath]
walkDirectory path = do
    children <- listDirectory path
    let fullChildren = map (path </>) children
    concatMapM walk fullChildren
