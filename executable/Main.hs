module Main where

import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import Errata
import Errata.Styles
import Data.Void (Void)
import Text.Megaparsec (SourcePos(..), unPos, ParseErrorBundle, attachSourcePos, errorOffset, bundleErrors, bundlePosState, parseErrorTextPretty)
import Parser (parseProgram)
import TypeChecker
import qualified Data.List.NonEmpty as NE

main :: IO ()
main = do
    content <- readFile "test.btl"
    case parseProgram content of
        Left e  -> TL.putStrLn $ prettyErrors (T.pack content) (parseErrorToErrata "test.btl" e)
        Right program -> case typeCheckProgram program of
            Left e  -> TL.putStrLn $ prettyErrors (T.pack content) [typeErrorToErrata "test.btl" e]
            Right _ -> putStrLn "successful"

typeErrorToErrata :: FilePath -> TypeError -> Errata
typeErrorToErrata path (TypeError pos msg) =
    errataSimple
        Nothing
        (blockSimple' fancyStyle basicPointer
            path
            Nothing
            (unPos (sourceLine pos), unPos (sourceColumn pos), Just "here")
            (Just $ red "type error: " <> (T.pack msg)))
        Nothing

parseErrorToErrata :: FilePath -> ParseErrorBundle String Void -> [Errata]
parseErrorToErrata path bundle =
    map toErrata . NE.toList $ fst $
        attachSourcePos errorOffset (bundleErrors bundle) (bundlePosState bundle)
    where
    toErrata (err, pos) =
        errataSimple
            Nothing
            (blockSimple' fancyStyle basicPointer
                path
                Nothing
                (unPos (sourceLine pos), unPos (sourceColumn pos), Just "here")
                (Just $ red "parse error: " <> (T.pack $ init $ parseErrorTextPretty err)))
            Nothing

red :: T.Text -> T.Text
red text = "\ESC[31m" <> text <> "\ESC[0m"
