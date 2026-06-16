{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Eta reduce" -}
{- HLINT ignore "Avoid lambda" -}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Parser (parseProgram) where

import Tokenizer
import AST
import Text.Parsec hiding (eof, char, newline, satisfy)
import Data.Char (isAlpha, isDigit)
import Data.Map (insert, empty, fromList)

type Parser = Parsec [Token] ()

parseProgram :: [Token] -> Either String (Expression ())
parseProgram tokens = do
    result <- case parse program "" tokens of
        Left err -> Left (show err)
        Right expression -> Right expression
    return result

program :: Parser (Expression ())
program = do
    result <- returnExpression
    match TokenSemicolon
    match TokenEOF
    return result

returnExpression :: Parser (Expression ())
returnExpression = assignment <|> typeAlias <|> ifExpression <|> returnValue

assignment :: Parser (Expression ())
assignment = do
    name <- symbol
    varFunctionAssignment name <|> tupleDestructuring name

typeAlias :: Parser (Expression ())
typeAlias = do
    (AliasedType name) <- typeSymbol
    match TokenAssign
    definition <- parseType
    match TokenSemicolon
    ensuing <- returnExpression
    return $ TypeLet name definition ensuing ()

varFunctionAssignment :: String -> Parser (Expression ())
varFunctionAssignment name = do
    value <- variableAssignment <|> function
    match TokenSemicolon
    ensuing <- returnExpression
    return $ Let name value ensuing ()

ifExpression :: Parser (Expression ())
ifExpression = do
    match TokenIf
    condition <- expression
    match TokenColon
    thenBranch <- returnExpression
    match TokenSemicolon
    elseBranch <- returnExpression
    return $ If condition thenBranch elseBranch ()

returnValue :: Parser (Expression ())
returnValue = do
    match TokenReturn
    exp <- expression
    return exp

variableAssignment :: Parser (Expression ())
variableAssignment = do
    match TokenAssign
    expression

function :: Parser (Expression ())
function = do
    match TokenLeftParenthesis
    argument <- symbol
    match TokenColon
    argumentType <- parseType
    (returnType, body) <- functionContinuation <|> functionEnd
    return $ Function argumentType returnType argument body

functionContinuation :: Parser (Type, Expression ())
functionContinuation = do
    match TokenComma
    argument <- symbol
    match TokenColon
    argumentType <- parseType
    (returnType, body) <- functionContinuation <|> functionEnd
    return $ (FunctionType argumentType returnType, Function argumentType returnType argument body)

functionEnd :: Parser (Type, Expression ())
functionEnd = do
    match TokenRightParenthesis
    match TokenColon
    returnType <- parseType
    match TokenAssign
    body <- returnExpression
    return (returnType, body)

tupleDestructuring :: String -> Parser (Expression ())
tupleDestructuring name = do
    tupleDestructuringContinuation [name]

tupleDestructuringContinuation :: [String] -> Parser (Expression ())
tupleDestructuringContinuation destructuredNames = do
    match TokenComma
    nextName <- symbol
    let destructuredNames' = destructuredNames ++ [nextName]
    tupleDestructuringContinuation destructuredNames' <|> tupleDestructuringEnd destructuredNames'

tupleDestructuringEnd :: [String] -> Parser (Expression ())
tupleDestructuringEnd destructuredNames = do
    match TokenAssign
    value <- expression
    match TokenSemicolon
    ensuing <- returnExpression
    return $ TupleDestructuring destructuredNames value ensuing ()

expression :: Parser (Expression ())
expression = chainl1 logical tupleConstructor
    where tupleConstructor = do
            match TokenComma
            return buildTuple

          buildTuple (Tuple expressions ()) newExpression = Tuple (expressions ++ [newExpression]) ()
          buildTuple left right = Tuple [left, right] ()

logical :: Parser (Expression ())
logical = chainl1 arithmetic logicOperation
    where logicOperation = do
            token <- match TokenEquality <|> match TokenLessThan <|> match TokenGreaterThan <|> match TokenLessThanEqual <|> match TokenGreaterThanEqual
            let operator = case token of
                    TokenEquality -> "=="
                    TokenLessThan -> "<"
                    TokenGreaterThan -> ">"
                    TokenLessThanEqual -> "<="
                    TokenGreaterThanEqual -> ">="
                    _ -> error "not reachable"
            return (\left right -> Application (Application (Variable operator ()) left ()) right ())

arithmetic :: Parser (Expression ())
arithmetic = chainl1 atom arithmeticOperation
    where arithmeticOperation = do
            token <- match TokenPlus <|> match TokenMinus
            let operator = case token of
                    TokenPlus -> "+"
                    TokenMinus -> "-"
                    _ -> error "not reachable"
            return (\left right -> Application (Application (Variable operator ()) left ()) right ())

atom :: Parser (Expression ())
-- try is used for lambda function because their beginning shares a few terms
-- with some of the productions of an expression that starts by referencing a
-- variable, and it is difficult to factor it out
atom = try lambda <|> variableUsage <|> unaryMinus <|> struct <|> integer <|> boolean <|> parenthesizedExpression

-- This takes care of things that might continue atoms, such as expression calls
-- and member access
atomContinuation :: Expression () -> Parser (Expression ())
atomContinuation atom = functionCall atom <|> structAccess atom <|> return atom

functionCall :: Expression () -> Parser (Expression ())
functionCall base = do
    match TokenLeftParenthesis
    argument <- logical
    let call = (Application base argument ())
    functionCallContinuation call <|> functionCallEnd call

functionCallContinuation :: Expression () -> Parser (Expression ())
functionCallContinuation base = do
    match TokenComma
    argument <- logical
    let call = (Application base argument ())
    functionCallContinuation call <|> functionCallEnd call

functionCallEnd :: Expression () -> Parser (Expression ())
functionCallEnd base = do
    match TokenRightParenthesis
    atomContinuation base

structAccess :: Expression () -> Parser (Expression ())
structAccess base = do
    match TokenPeriod
    name <- symbol
    atomContinuation $ StructAccess name base ()

struct :: Parser (Expression ())
struct = do
    match TokenLeftCurlyBrace
    members <- sepEndBy1 structMember (match TokenComma)
    match TokenRightCurlyBrace

    return $ Struct (fromList members) ()

structMember :: Parser (String, Expression ())
structMember = do
    name <- symbol
    match TokenColon
    value <- logical
    return (name, value)

lambda :: Parser (Expression ())
lambda = do
    match TokenLeftParenthesis
    argument <- symbol
    match TokenColon
    argumentType <- parseType
    (returnType, body) <- lambdaContinuation <|> lambdaEnd
    return $ Function argumentType returnType argument body

lambdaContinuation :: Parser (Type, Expression ())
lambdaContinuation = do
    match TokenComma
    argument <- symbol
    match TokenColon
    argumentType <- parseType
    (returnType, body) <- lambdaContinuation <|> lambdaEnd
    return $ (FunctionType argumentType returnType, Function argumentType returnType argument body)

lambdaEnd :: Parser (Type, Expression ())
lambdaEnd = do
    match TokenRightParenthesis
    match TokenColon
    returnType <- parseType
    match TokenAssign
    body <- logical
    return (returnType, body)

variableUsage :: Parser (Expression ())
variableUsage = do
    name <- symbol
    let base = Variable name ()
    atomContinuation base

unaryMinus :: Parser (Expression())
unaryMinus = do
    match TokenMinus
    content <- atom
    return $ Application (Application (Variable "-" ()) (Integer 0) ()) content ()

parenthesizedExpression :: Parser (Expression ())
parenthesizedExpression = do
    match TokenLeftParenthesis
    content <- expression
    match TokenRightParenthesis
    atomContinuation content

parseType :: Parser Type
parseType = chainr1 baseTypes functionType
    where baseTypes = booleanType <|> integerType <|> typeSymbol <|> parenthesizedType <|> structType
          functionType = do
            match TokenArrow
            return (\left right -> FunctionType left right)

tupleType :: Type -> Parser Type
tupleType t = tupleTypeContinuation [t]

tupleTypeContinuation :: [Type] -> Parser Type
tupleTypeContinuation ts = do
    match TokenComma
    nextType <- parseType
    let newTypes = (ts ++ [nextType])
    tupleTypeContinuation newTypes <|> tupleTypeEnd newTypes

tupleTypeEnd :: [Type] -> Parser Type
tupleTypeEnd ts = return $ TupleType ts

structType :: Parser Type
structType = do
    match TokenLeftCurlyBrace
    members <- sepEndBy1 structMemberType (match TokenComma)
    match TokenRightCurlyBrace

    return $ StructType (fromList members)

structMemberType :: Parser (String, Type)
structMemberType = do
    name <- symbol
    match TokenColon
    t <- parseType
    return (name, t)

booleanType :: Parser Type
booleanType = do
    match TokenTypeBoolean
    return BooleanType

integerType :: Parser Type
integerType = do
    match TokenTypeInteger
    return IntegerType

parenthesizedType :: Parser Type
parenthesizedType = do
    match TokenLeftParenthesis
    t <- parseType
    t <- tupleType t <|> (return t)
    match TokenRightParenthesis
    return t

symbol :: Parser String
symbol = satisfy (\t -> case t of
    TokenSymbol name -> Just name
    _  -> Nothing)

typeSymbol :: Parser Type
typeSymbol = satisfy (\t -> case t of
    TokenTypeSymbol name -> Just (AliasedType name)
    _  -> Nothing)

integer :: Parser (Expression ())
integer = satisfy (\t -> case t of
    TokenIntegerLiteral value -> Just (Integer value)
    _  -> Nothing)

boolean :: Parser (Expression ())
boolean = satisfy (\t -> case t of
    TokenBooleanLiteral value -> Just (Boolean value)
    _  -> Nothing)

match :: Token -> Parser Token
match t = satisfy (\t' -> if t == t' then Just t else Nothing)

satisfy :: (Token -> Maybe a) -> Parser a
satisfy f = tokenPrim show (\pos _ _ -> pos) f
