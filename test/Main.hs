module Main where

import Parser (parseProgram)

import Control.Monad.Extra (concatMapM)
import Data.List (isSuffixOf)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
    let runner = defaultMain . testGroup "parse"

    files <- getFiles "tests"
    testResults <- mapM testParsing files
    runner testResults

testParsing :: FilePath -> IO TestTree
testParsing path = do
    content <- readFile path
    return $ testCase path $ case parseProgram content of
        Right _ -> return ()
        Left error -> assertFailure (errorBundlePretty error)


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
