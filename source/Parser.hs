{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Parser (parseProgram) where

import AST
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative
import Data.Void
import Data.Map (fromList)


parseProgram :: String -> Either (M.ParseErrorBundle String Void) SourceExpression
parseProgram input = M.parse program "" input


type Parser = M.Parsec Void String

program :: Parser SourceExpression
program = do
    content <- returnExpression
    symbol ";"
    M.eof
    return content

returnExpression :: Parser SourceExpression
returnExpression = binding <|> ifExpression <|> returnValue

-- This parser is used to abstract the logic of parsing a ";" and a collection
-- of ensuing expression from assignments and type declarations.
binding :: Parser SourceExpression
binding = do
    expressionBuilder <- typeDeclaration <|> assignment
    symbol ";"
    body <- returnExpression
    return $ expressionBuilder body

ifExpression :: Parser SourceExpression
ifExpression = do
    position <- M.getSourcePos
    symbol "if"
    condition <- expression
    symbol ":"
    left <- expression
    symbol ";"
    right <- expression
    return $ If condition left right position

returnValue :: Parser SourceExpression
returnValue = do
    symbol "return"
    value <- expression
    return value

typeDeclaration :: Parser (SourceExpression -> SourceExpression)
typeDeclaration = do
    position <- M.getSourcePos
    name <- typeIdentifier
    symbol "="
    aliasedType <- typeParser
    return $ \body -> TypeDeclaration name aliasedType body position

assignment :: Parser (SourceExpression -> SourceExpression)
assignment = do
    position <- M.getSourcePos
    names <- M.sepBy1 identifier (symbol ",")
    symbol "="
    value <- expression
    case names of
        [name] -> return $ \body -> Assignment name value body position
        _ -> return $ \body -> TupleDestructuring names value body position


expression :: Parser SourceExpression
expression = error "not implemented"

typeParser :: Parser Type
typeParser = do
    atomic <- atomicTypes
    functionType atomic <|> return atomic

atomicTypes :: Parser Type
atomicTypes = booleanType <|> integerType <|> userType <|> parenthesizedType <|> recordType

functionType :: Type -> Parser Type
functionType argumentType = do
    symbol "->"
    returnType <- typeParser
    return $ FunctionType argumentType returnType

booleanType :: Parser Type
booleanType = do
    symbol "boolean"
    return BooleanType

integerType :: Parser Type
integerType = do
    symbol "integer"
    return IntegerType

userType :: Parser Type
userType = do
    name <- typeIdentifier
    return $ UserType name

parenthesizedType :: Parser Type
parenthesizedType = do
    symbol "("
    content <- typeParser
    symbol ")"
    return content

recordType :: Parser Type
recordType = do
    symbol "{"
    members <- M.sepEndBy1 recordMemberType (symbol ",")
    symbol "}"
    return $ RecordType (fromList members)

recordMemberType :: Parser (String, Type)
recordMemberType = do
    name <- identifier
    symbol ":"
    t <- typeParser
    return (name, t)

tupleType :: Parser Type
tupleType = error "not implemented"

typeIdentifier :: Parser String
typeIdentifier = error "not implemented"

identifier :: Parser String
identifier = error "not implemented"

-- Helper functions

space :: Parser ()
space = L.space (C.space1) (L.skipLineComment "--") M.empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: String -> Parser String
symbol = L.symbol space
