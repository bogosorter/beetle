module Errors (showParseError, showTypeError) where

import Data.Void
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import Errata
import Errata.Styles
import Text.Megaparsec
import TypeChecker
import qualified Data.List.NonEmpty as NE

showParseError :: FilePath -> String -> ParseErrorBundle String Void -> IO ()
showParseError path content e = TL.putStrLn $ prettyErrors (T.pack content) (parseErrorToErrata path e)

showTypeError :: FilePath -> String -> TypeError -> IO ()
showTypeError path content e = TL.putStrLn $ prettyErrors (T.pack content) [typeErrorToErrata path e]

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
