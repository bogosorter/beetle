-- Somewhat vibe-coded

module Main where

import SimpleCompiler (compile)

import Control.Monad.Extra (concatMapM)
import Data.List (isSuffixOf)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), replaceExtension)
import System.Process (readProcess)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = do
    files <- getFiles "tests"
    executionTests <- mapM testExecution files
    defaultMain $ testGroup "all" executionTests

testExecution :: FilePath -> IO TestTree
testExecution path = do
    let executablePath = replaceExtension path ""
    let expectedPath = replaceExtension path "out"
    expected <- readFile expectedPath
    return $ testCase path $ do
        compile path executablePath
        actual <- readProcess executablePath [] ""
        actual @?= expected

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
