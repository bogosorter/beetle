{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser (parseProgram) where

import AST
import BeetlePrelude

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
    space
    content <- returnExpression
    symbol ";"
    M.eof
    return $ prelude content

returnExpression :: Parser SourceExpression
returnExpression = binding <|> ifExpression <|> returnValue

-- This parser is used to abstract the logic of parsing a ";" and a collection
-- of ensuing expression from assignments and type declarations.
binding :: Parser SourceExpression
binding = do
    expressionBuilder <- typeAssignment <|> assignment
    symbol ";"
    body <- returnExpression
    return $ expressionBuilder body

ifExpression :: Parser SourceExpression
ifExpression = do
    position <- M.getSourcePos
    keyword "if"
    condition <- expression
    simpleIf position condition <|> ifLet position condition

simpleIf :: M.SourcePos -> SourceExpression -> Parser SourceExpression
simpleIf position condition = do
    symbol ":"
    left <- returnExpression
    symbol ";"
    right <- returnExpression
    return $ If condition left right position

ifLet :: M.SourcePos -> SourceExpression -> Parser SourceExpression
ifLet position condition = do
    assertionPosition <- M.getSourcePos
    keyword "is"
    constructor <- typeIdentifier
    symbol ":"
    left <- returnExpression
    symbol ";"
    right <- returnExpression
    return $ If (TypeAssertion condition constructor assertionPosition) left right position


returnValue :: Parser SourceExpression
returnValue = do
    keyword "return"
    value <- expression
    return value

typeAssignment :: Parser (SourceExpression -> SourceExpression)
typeAssignment = do
    position <- M.getSourcePos
    name <- typeIdentifier
    symbol "="
    t <- typeParser

    finalType <- case t of
        UserType constructor -> attemptSumType constructor <|> return t
        _ -> return t
    return $ \body -> TypeAssignment name finalType body position

attemptSumType :: String -> Parser Type
attemptSumType firstConstructor = do
    firstType <- typeParser
    symbol "|"

    additionalConstructors <- M.sepBy1 (do
            constructor <- typeIdentifier
            t <- typeParser
            return (constructor, t)
        ) (symbol "|")

    return $ SumType (fromList $ (firstConstructor, firstType) : additionalConstructors)

assignment :: Parser (SourceExpression -> SourceExpression)
assignment = do
    position <- M.getSourcePos
    names <- M.sepBy1 (identifier <|> hole) (symbol ",")
    case names of
        [name] -> singleAssignment position name <|> functionDefinition position name
        _ -> tupleAssignment position names

singleAssignment :: M.SourcePos -> String -> Parser (SourceExpression -> SourceExpression)
singleAssignment position name = do
    symbol "="
    value <- expression
    return $ \body -> Assignment name value body position

tupleAssignment :: M.SourcePos -> [String] -> Parser (SourceExpression -> SourceExpression)
tupleAssignment position names = do
    symbol "="
    value <- expression
    return $ \body -> TupleDestructuring names value body position

functionDefinition :: M.SourcePos -> String -> Parser (SourceExpression -> SourceExpression)
functionDefinition position name = do
    symbol "("
    arguments <- M.sepBy1 identifierTypePair (symbol ",")
    symbol ")"
    symbol ":"
    returnType <- typeParser
    symbol "="
    body <- returnExpression

    let builder :: (String, Type) -> (SourceExpression, Type) -> (SourceExpression, Type)
        builder (argumentName, argumentType) (body, bodyType) =
            (Function argumentType bodyType argumentName body position, FunctionType argumentType bodyType)

    let (result, _) = foldr builder (body, returnType) arguments
    return $ \body -> Assignment name result body position

expression :: Parser SourceExpression
expression = do
    position <- M.getSourcePos
    logicals <- M.sepBy1 binaryOperation (symbol ",")
    case logicals of
        [single] -> return single
        many -> return $ Tuple many position

binaryOperation :: Parser SourceExpression
binaryOperation = do
    position <- M.getSourcePos
    E.makeExprParser atom (table position)

    where table position =
            [
                [ E.InfixL (builder position <$> symbol "*")
                , E.InfixL (builder position <$> symbol "/")
                , E.InfixL (builder position <$> symbol "mod")
                , E.InfixL (builder position <$> symbol "rem")
                ]
            ,
                [ E.InfixL (builder position <$> symbol "+")
                , E.InfixL (builder position <$> symbol "-")
                ]
            ,
                [ E.InfixL (builder position <$> symbol "==")
                , E.InfixL (builder position <$> symbol "<=")
                , E.InfixL (builder position <$> symbol ">=")
                , E.InfixL (builder position <$> symbol "<")
                , E.InfixL (builder position <$> symbol ">")
                ]
            ,
                [ E.InfixL (builder position <$> symbol "and")
                ]
            ,
                [ E.InfixL (builder position <$> symbol "or")
                ]
            ]
          builder position operation left right = Application (Application (Variable operation position) left position) right position

atom :: Parser SourceExpression
atom = M.try lambda <|> variableUsage <|> unaryMinus <|> logicalNot <|> constructorParser <|> recordParser <|> integer <|> boolean <|> parenthesizedExpression

-- This takes care of things that might continue atoms, such as expression calls
-- and member access
atomContinuation :: SourceExpression -> Parser (SourceExpression)
atomContinuation atom = functionCall atom <|> recordAccess atom <|> return atom

lambda :: Parser SourceExpression
lambda = do
    position <- M.getSourcePos
    symbol "("
    names <- M.sepBy1 identifierTypePair (symbol ",")
    symbol ")"
    symbol ":"
    returnType <- typeParser
    symbol "="
    body <- binaryOperation

    let builder :: (String, Type) -> (SourceExpression, Type) -> (SourceExpression, Type)
        builder (argumentName, argumentType) (body, bodyType) =
            (Function argumentType bodyType argumentName body position, FunctionType argumentType bodyType)

    let (result, _) = foldr builder (body, returnType) names
    return result

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

logicalNot :: Parser SourceExpression
logicalNot = do
    position <- M.getSourcePos
    symbol "not"
    inner <- atom
    return $ Application (Variable "not" position) inner position

constructorParser :: Parser SourceExpression
constructorParser = do
    position <- M.getSourcePos
    constructor <- typeIdentifier
    value <- atom
    return $ Constructor constructor value position

recordParser :: Parser SourceExpression
recordParser = do
    position <- M.getSourcePos
    symbol "{"
    members <- M.sepEndBy recordMember (symbol ",")
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
    value <- lexeme L.decimal
    return $ Integer value position

boolean :: Parser SourceExpression
boolean = true <|> false

parenthesizedExpression :: Parser SourceExpression
parenthesizedExpression = do
    symbol "("
    value <- expression
    symbol ")"
    atomContinuation value

functionCall :: SourceExpression -> Parser SourceExpression
functionCall base = do
    symbol "("
    -- we do not use full expressions to avoid ambiguity with tuples
    arguments <- M.sepBy1 binaryOperation (symbol ",")
    symbol ")"

    let buildCall base argument = Application base argument (getPosition base)
    let result = foldl buildCall base arguments
    atomContinuation result

recordAccess :: SourceExpression -> Parser SourceExpression
recordAccess base = do
    symbol "."
    name <- identifier
    let result = RecordMember base name (getPosition base)

    atomContinuation result

true :: Parser SourceExpression
true = do
    position <- M.getSourcePos
    keyword "true"
    return $ Boolean True position

false :: Parser SourceExpression
false = do
    position <- M.getSourcePos
    keyword "false"
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
    keyword "Boolean"
    return BooleanType

integerType :: Parser Type
integerType = do
    keyword "Integer"
    return IntegerType

userType :: Parser Type
userType = do
    name <- typeIdentifier
    return $ UserType name

parenthesizedType :: Parser Type
parenthesizedType = do
    symbol "("
    content <- tupleType
    symbol ")"
    return content

recordType :: Parser Type
recordType = do
    symbol "{"
    members <- M.sepEndBy identifierTypePair (symbol ",")
    symbol "}"
    return $ RecordType (fromList members)

identifierTypePair :: Parser (String, Type)
identifierTypePair = do
    name <- identifier
    symbol ":"
    t <- typeParser
    return (name, t)

tupleType :: Parser Type
tupleType = do
    types <- M.sepBy1 typeParser (symbol ",")
    case types of
        [t] -> return t
        _ -> return $ TupleType types

typeIdentifier :: Parser String
typeIdentifier = lexeme $ do
    first <- C.upperChar
    following <- M.many C.letterChar
    return (first : following)

reserved :: [String]
reserved = ["if", "case", "of", "return", "true", "false", "boolean", "integer", "mod", "rem", "not", "and", "or", "is"]

identifier :: Parser String
identifier = M.try $ lexeme $ do
    first <- C.lowerChar
    following <- M.many C.letterChar
    let word = first : following
    if word `elem` reserved
        then fail $ "keyword " ++ show word ++ " used as an identifier"
        else return word

hole :: Parser String
hole = symbol "_"

-- Helper functions

space :: Parser ()
space = L.space (C.space1) (L.skipLineComment "--") M.empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: String -> Parser String
symbol = L.symbol space

keyword :: String -> Parser ()
keyword w = M.try $ lexeme $ do
    C.string w
    M.notFollowedBy C.alphaNumChar
    return ()
