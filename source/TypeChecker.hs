module TypeChecker (typeCheckProgram) where

import AST
import Text.Megaparsec (SourcePos)

data TypeError = TypeError SourcePos String

typeCheckProgram :: SourceExpression -> Either TypeError TypedExpression
typeCheckProgram program = do
    checked <- typeCheck program
    case getType checked of
        IntegerType -> Right checked
        BooleanType -> Right checked
        _ -> Left $ TypeError (getPosition program) "every program must return an integer or a boolean"

typeCheck :: SourceExpression -> Either TypeError TypedExpression
typeCheck = error "not implemented"


instance Show TypeError where
    show (TypeError _ e) = e
