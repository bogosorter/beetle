{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Parser (parseProgram) where

import AST
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Monad.Combinators.Expr as E
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
expression = do
    position <- M.getSourcePos
    logicals <- M.sepBy binaryOperation (symbol ",")
    case logicals of
        [single] -> return single
        many -> return $ Tuple many position

binaryOperation :: Parser SourceExpression
binaryOperation = do
    position <- M.getSourcePos
    E.makeExprParser atom (table position)

    where table position =
            [
                [ E.InfixL (builder position <$> symbol "+")
                , E.InfixL (builder position <$> symbol "-")
                ]
            ,
                [ E.InfixL (builder position <$> symbol "==")
                , E.InfixL (builder position <$> symbol "<")
                , E.InfixL (builder position <$> symbol ">")
                , E.InfixL (builder position <$> symbol "<=")
                , E.InfixL (builder position <$> symbol ">=")
                ]
            ]
          builder position operation left right = Application (Application (Variable operation position) left position) right position

atom :: Parser SourceExpression
atom = M.try lambda <|> variableUsage <|> unaryMinus <|> recordParser <|> integer <|> boolean <|> parenthesizedExpression

-- This takes care of things that might continue atoms, such as expression calls
-- and member access
atomContinuation :: SourceExpression -> Parser (SourceExpression)
atomContinuation atom = functionCall atom <|> recordAccess atom <|> return atom

lambda :: Parser SourceExpression
lambda = do error "not implemented"

variableUsage :: Parser SourceExpression
variableUsage = do
    position <- M.getSourcePos
    name <- identifier
    atomContinuation (Variable name position)

unaryMinus :: Parser SourceExpression
unaryMinus = do
    position <- M.getSourcePos
    symbol "-"
    inner <- atom
    return $ Application (Application (Variable "-" position) (Integer 0 position) position) inner position

recordParser :: Parser SourceExpression
recordParser = do
    position <- M.getSourcePos
    symbol "{"
    members <- M.sepEndBy1 recordMember (symbol ",")
    symbol "}"

    return $ Record (fromList members) position

recordMember :: Parser (String, SourceExpression)
recordMember = do
    name <- identifier
    symbol ":"
    value <- binaryOperation
    return (name, value)

integer :: Parser SourceExpression
integer = do
    position <- M.getSourcePos
    value <- L.decimal
    return $ Integer value position

boolean :: Parser SourceExpression
boolean = true <|> false

parenthesizedExpression :: Parser SourceExpression
parenthesizedExpression = do
    symbol "("
    value <- expression
    symbol ")"
    return $ value

functionCall :: SourceExpression -> Parser SourceExpression
functionCall base = do
    position <- M.getSourcePos

    symbol "("
    -- we do not use full expressions to avoid ambiguity with tuples
    arguments <- M.sepBy binaryOperation (symbol ",")
    symbol ")"

    let buildCall base argument = Application base argument position
    let result = foldl buildCall base arguments
    atomContinuation result

recordAccess :: SourceExpression -> Parser SourceExpression
recordAccess base = do
    position <- M.getSourcePos

    symbol "."
    name <- identifier
    let result = RecordMember base name position

    atomContinuation result

true :: Parser SourceExpression
true = do
    position <- M.getSourcePos
    symbol "true"
    return $ Boolean True position

false :: Parser SourceExpression
false = do
    position <- M.getSourcePos
    symbol "false"
    return $ Boolean False position

typeParser :: Parser Type
typeParser = do
    atomic <- atomicTypes -- not a general expression because using tuples here would lead to ambiguity
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
