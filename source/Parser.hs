{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE TemplateHaskell #-}

module Parser (parseProgram) where

import AST

import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Monad.Combinators.Expr as E
import Control.Applicative ((<|>))
import Data.Void
import Data.Map (fromList, empty)
import Data.Char (ord, isAscii)
import Data.FileEmbed (embedFileRelative)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)

parseProgram :: String -> Either (M.ParseErrorBundle String Void) SourceExpression
parseProgram input = do
    programContent <- M.parse program "" input
    M.parse (moduleParser programContent) "prelude.btl" prelude


type Parser = M.Parsec Void String

program :: Parser SourceExpression
program = do
    space
    content <- returnExpression
    M.eof
    return content

moduleParser :: SourceExpression -> Parser SourceExpression
moduleParser program = do
    space
    content <- exportExpression program
    M.eof
    return content

returnExpression :: Parser SourceExpression
returnExpression = binding <|> ifExpression <|> returnValue <|> match

exportExpression :: SourceExpression -> Parser SourceExpression
exportExpression program = exportBinding program <|> exportValue program

-- This parser is used to abstract the logic of parsing a ";" and a collection
-- of ensuing expression from assignments and type declarations.
binding :: Parser SourceExpression
binding = do
    expressionBuilder <- alias <|> typeAssignment <|> assignment <|> unwrap
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
    (left, right) <- ifBody
    return $ If condition left right position

ifLet :: M.SourcePos -> SourceExpression -> Parser SourceExpression
ifLet position scrutinee = do
    keyword "is"
    constructor <- typeIdentifier <|> stringNil <|> listNil
    introducedVariable <- symbol "(" *> identifier <* symbol ")" <|> return "_"
    (left, right) <- ifBody
    return $ IfLet scrutinee constructor introducedVariable position left right position

ifBody :: Parser (SourceExpression, SourceExpression)
ifBody = do
    left <- (symbol ":" *> returnValue) <|> scope
    right
        <- (keyword "else" *> ((symbol ":" *> returnValue) <|> scope))
        <|> returnExpression
    return (left, right)


returnValue :: Parser SourceExpression
returnValue = do
    keyword "return"
    value <- expression
    symbol ";"
    return value

-- This parser is used to abstract the logic of parsing a ";" and a collection
-- of ensuing expression from assignments and type declarations.
exportBinding :: SourceExpression -> Parser SourceExpression
exportBinding program = do
    expressionBuilder <- alias <|> typeAssignment <|> assignment
    body <- exportExpression program
    return $ expressionBuilder body

exportValue :: SourceExpression -> Parser SourceExpression
exportValue program = do
    keyword "export"
    symbol ";"
    return program

alias :: Parser (SourceExpression -> SourceExpression)
alias = do
    position <- M.getSourcePos
    keyword "alias"
    name <- typeIdentifier
    symbol "for"
    t <- typeParser
    symbol ";"
    return $ \body -> TypeAssignment name t body position

typeAssignment :: Parser (SourceExpression -> SourceExpression)
typeAssignment = do
    position <- M.getSourcePos
    keyword "type"
    name <- typeIdentifier
    parameters <- typeParameters <|> return []
    constructors <- sumType <|> newType name

    let t = SumType [name | (TypeVariable name) <- parameters] (fromList constructors)
    return $ \body -> TypeAssignment name t body position

    where sumType :: Parser [(String, Type)]
          sumType =  do
            symbol "{"
            constructors <- M.sepBy optionConstructor (symbol ",")
            symbol "}"
            return constructors
          newType :: String -> Parser [(String, Type)]
          newType name = do
            symbol "("
            t <- typeParser
            symbol ")"
            symbol ";"
            return [(name, t)]

optionConstructor :: Parser (String, Type)
optionConstructor = do
    name <- typeIdentifier
    t <- (symbol "(" *> typeParser <* symbol ")") <|> (return $ RecordType empty)
    return (name, t)

assignment :: Parser (SourceExpression -> SourceExpression)
assignment = do
    position <- M.getSourcePos
    names <- M.sepBy1 (identifier <|> hole <|> operatorIdentifier) (symbol ",")
    case names of
        [name] -> singleAssignment position name <|> functionDefinition position name
        _ -> tupleAssignment position names

unwrap :: Parser (SourceExpression -> SourceExpression)
unwrap = do
    position <- M.getSourcePos
    constructor <- typeIdentifier
    symbol "("
    name <- identifier
    symbol ")"
    symbol "="
    scrutinee <- expression
    symbol ";"
    return $ \body -> Unwrap name position scrutinee constructor body position

singleAssignment :: M.SourcePos -> String -> Parser (SourceExpression -> SourceExpression)
singleAssignment position name = do
    symbol "="
    value <- expression
    symbol ";"
    return $ \body -> Assignment name value body position

tupleAssignment :: M.SourcePos -> [String] -> Parser (SourceExpression -> SourceExpression)
tupleAssignment position names = do
    symbol "="
    value <- expression
    symbol ";"
    return $ \body -> TupleDestructuring names value body position

functionDefinition :: M.SourcePos -> String -> Parser (SourceExpression -> SourceExpression)
functionDefinition position name = do
    symbol "("
    arguments <- M.sepBy1 identifierTypePair (symbol ",")
    symbol ")"
    symbol "->"
    returnType <- typeParser
    body <- scope

    let builder :: (String, Type) -> (SourceExpression, Type) -> (SourceExpression, Type)
        builder (argumentName, argumentType) (body, bodyType) =
            (Function argumentType bodyType argumentName body position, FunctionType argumentType bodyType)

    let (result, _) = foldr builder (body, returnType) arguments
    return $ \body -> Assignment name result body position

match :: Parser SourceExpression
match = do
    position <- M.getSourcePos

    keyword "match"
    scrutinee <- expression
    symbol "{"
    branches <- M.sepBy1 branchParser (symbol ",")
    symbol "}"

    return $ Match scrutinee branches Nothing position

    where branchParser :: Parser (String, String, M.SourcePos, SourceExpression)
          branchParser = do
            position <- M.getSourcePos
            constructor <- typeIdentifier
            introducedVariable <- symbol "(" *> identifier <* symbol ")" <|> return "_"
            body <- (symbol "->" *> nonTupleExpression) <|> scope
            return (constructor, introducedVariable, position, body)

expression :: Parser SourceExpression
expression = do
    position <- M.getSourcePos
    logicals <- M.sepBy1 nonTupleExpression (symbol ",")
    case logicals of
        [single] -> return single
        many -> return $ Tuple many position

nonTupleExpression :: Parser SourceExpression
nonTupleExpression = binaryOperation <|> match

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
                [ E.InfixR (constructor position <$ symbol "::")
                , E.InfixR (builder position <$> symbol ">>")
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
          constructor position left right = Constructor "ListConstructor" (Tuple [left, right] position) position

atom :: Parser SourceExpression
atom = M.try lambda <|> variableUsage <|> unaryMinus <|> logicalNot <|> constructorParser <|> M.try recordParser <|> scope <|> integer <|> boolean <|> characterExpression <|> list <|> string <|> parenthesizedExpression

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
    symbol "->"
    returnType <- typeParser
    symbol "="
    body <- nonTupleExpression

    let builder :: (String, Type) -> (SourceExpression, Type) -> (SourceExpression, Type)
        builder (argumentName, argumentType) (body, bodyType) =
            (Function argumentType bodyType argumentName body position, FunctionType argumentType bodyType)

    let (result, _) = foldr builder (body, returnType) names
    return result

scope :: Parser SourceExpression
scope = do
    symbol "{"
    content <- returnExpression
    symbol "}"
    return content

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
    value <- (symbol "(" *> nonTupleExpression <* symbol ")") <|> (return $ Record empty position)
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
    value <- nonTupleExpression
    return (name, value)

integer :: Parser SourceExpression
integer = do
    position <- M.getSourcePos
    value <- lexeme L.decimal
    return $ Integer value position

boolean :: Parser SourceExpression
boolean = true <|> false

string :: Parser SourceExpression
string = lexeme $ do
    position <- M.getSourcePos
    C.char '\''
    content <- M.many character
    C.char '\''

    let emptyString = EmptyString position
    let prependCharacter :: Char -> SourceExpression -> SourceExpression
        prependCharacter character string =
            Constructor "ListConstructor" (Tuple [Character (ord character) position, string] position) position

    return $ foldr prependCharacter emptyString content

list :: Parser SourceExpression
list = do
    position <- M.getSourcePos
    symbol "["
    members <- M.sepBy nonTupleExpression (symbol ",")
    symbol "]"

    let emptyList = EmptyList position
    let prependElement :: SourceExpression -> SourceExpression -> SourceExpression
        prependElement element list =
            Constructor "ListConstructor" (Tuple [element, list] position) position

    return $ foldr prependElement emptyList members

characterExpression :: Parser SourceExpression
characterExpression = lexeme $ do
    position <- M.getSourcePos
    C.char '"'
    content <- character
    C.char '"'

    return $ Character (ord content) position

character :: Parser Char
character = regularCharacter <|> escapedCharacter

regularCharacter :: Parser Char
regularCharacter = M.satisfy (\c -> isAscii c && c /= '\'' && c /= '\\')

escapedCharacter :: Parser Char
escapedCharacter = do
    _ <- C.char '\\'
    c <- C.asciiChar
    case c of
        '\'' -> return '\''
        '\\' -> return '\\'
        'n' -> return '\n'
        _ -> fail $ "unknown escaped character: \\" ++ [c]

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
    arguments <- M.sepBy1 nonTupleExpression (symbol ",")
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
atomicTypes = booleanType <|> integerType <|> characterType <|> typeVariable <|> userType <|> parenthesizedType <|> recordType <|> listType

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

characterType :: Parser Type
characterType = do
    keyword "Character"
    return CharacterType

userType :: Parser Type
userType = do
    name <- typeIdentifier
    arguments <- typeArguments <|> return []
    return $ UserType name arguments

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

listType :: Parser Type
listType = do
    symbol "["
    memberType <- typeParser
    symbol "]"
    return $ UserType "List" [memberType]

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

typeVariable :: Parser Type
typeVariable = do
    name <- identifier
    return $ TypeVariable name

typeParameters :: Parser [Type]
typeParameters = do
    symbol "<"
    parameters <- M.sepBy1 typeVariable (symbol ",")
    symbol ">"
    return parameters

typeArguments :: Parser [Type]
typeArguments = do
    symbol "<"
    parameters <- M.sepBy1 typeParser (symbol ",")
    symbol ">"
    return parameters

reserved :: [String]
reserved = ["if", "case", "of", "return", "true", "false", "boolean", "integer", "mod", "rem", "not", "and", "or", "is", "export", "match"]

operators :: [String]
operators = ["*", "/", "mod", "rem", "+", "-", "::", ">>", "==", "<=", ">=", "<", ">", "and", "or"]

identifier :: Parser String
identifier = M.try $ lexeme $ do
    first <- C.lowerChar
    following <- M.many C.letterChar
    let word = first : following
    if word `elem` reserved
        then fail $ "keyword " ++ show word ++ " used as an identifier"
        else return word

operatorIdentifier :: Parser String
operatorIdentifier = lexeme $ do
    C.char '('
    content <- M.manyTill character (C.char ')')
    if content `elem` operators
        then return content
        else fail $ show content ++ " is not an operator"

hole :: Parser String
hole = symbol "_"

stringNil :: Parser String
stringNil = do
    symbol "''"
    return "ListNil"

listNil :: Parser String
listNil = do
    symbol "[]"
    return "ListNil"

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


prelude :: String
prelude = (unpack . decodeUtf8) $(embedFileRelative "prelude/prelude.btl")
