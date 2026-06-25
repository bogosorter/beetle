module Main where

import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import Errata
import Errata.Styles
import Text.Megaparsec (SourcePos(..), unPos, errorBundlePretty)
import Parser (parseProgram)
import TypeChecker

main :: IO ()
main = do
    content <- readFile "test.btl"
    case parseProgram content of
        Left e  -> putStrLn (errorBundlePretty e)
        Right program -> case typeCheckProgram program of
            Left e  -> TL.putStrLn $ prettyErrors (T.pack content) [toErrata "test.btl" e]
            Right _ -> putStrLn "successful"

toErrata :: FilePath -> TypeError -> Errata
toErrata path (TypeError pos msg) =
    errataSimple
        Nothing
        (blockSimple' fancyStyle basicPointer
            path
            Nothing
            (unPos (sourceLine pos), unPos (sourceColumn pos), Just "here")
            (Just $ red "type error: " <> (T.pack msg)))
        Nothing

red :: T.Text -> T.Text
red text = "\ESC[31m" <> text <> "\ESC[0m"
