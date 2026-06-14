{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Eta reduce" -}
{- HLINT ignore "Avoid lambda" -}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Parser (parseProgram) where

import AST
import Text.Parsec hiding (eof, char, newline, satisfy)
import Data.Char (isAlpha, isDigit)
import Debug.Trace

type Parser = Parsec [Token] ()

data Token = TokenIntegerLiteral Int
           | TokenBooleanLiteral Bool
           | TokenSymbol String
           | TokenLeftParenthesis
           | TokenRightParenthesis
           | TokenComma
           | TokenSemicolon
           | TokenColon
           | TokenEquality
           | TokenAssign
           | TokenArrow
           | TokenGreaterThan
           | TokenLessThan
           | TokenGreaterThanEqual
           | TokenLessThanEqual
           | TokenPlus
           | TokenMinus
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenTypeBoolean
           | TokenTypeInteger
           | TokenReturn
           | TokenEOF
    deriving (Show, Eq)

tokenize :: String -> Either String [Token]
tokenize [] = Right [TokenEOF]
tokenize input@(c:cs)
    | c `elem` [' ', '\t', '\n', '\r'] = tokenize cs
    | take 2 input == "--" = do
        let rest = dropWhile (/= '\n') input
        tokenize rest
    | c == '(' = do
        rest <- tokenize cs
        return $ TokenLeftParenthesis : rest
    | c == ')' = do
        rest <- tokenize cs
        return $ TokenRightParenthesis : rest
    | c == ',' = do
        rest <- tokenize cs
        return $ TokenComma : rest
    | c == ';' = do
        rest <- tokenize cs
        return $ TokenSemicolon : rest
    | c == ':' = do
        rest <- tokenize cs
        return $ TokenColon : rest
    | take 2 input == "==" = do
        rest <- tokenize (drop 2 input)
        return $ TokenEquality : rest
    | c == '=' = do
        rest <- tokenize cs
        return $ TokenAssign : rest
    | take 2 input == "->" = do
        rest <- tokenize (drop 2 input)
        return $ TokenArrow : rest
    | take 2 input == ">=" = do
        rest <- tokenize (drop 2 input)
        return $ TokenGreaterThanEqual : rest
    | take 2 input == "<=" = do
        rest <- tokenize (drop 2 input)
        return $ TokenLessThanEqual : rest
    | c == '>' = do
        rest <- tokenize cs
        return $ TokenGreaterThan : rest
    | c == '<' = do
        rest <- tokenize cs
        return $ TokenLessThan : rest
    | c == '+' = do
        rest <- tokenize cs
        return $ TokenPlus : rest
    | c == '-' = do
        rest <- tokenize cs
        return $ TokenMinus : rest
    | take 2 input == "if" = do
        rest <- tokenize (drop 2 input)
        return $ TokenIf : rest
    | take 4 input == "then" = do
        rest <- tokenize (drop 4 input)
        return $ TokenThen : rest
    | take 4 input == "else" = do
        rest <- tokenize (drop 4 input)
        return $ TokenElse : rest
    | take 4 input == "true" = do
        rest <- tokenize (drop 4 input)
        return $ TokenBooleanLiteral True : rest
    | take 5 input == "false" = do
        rest <- tokenize (drop 5 input)
        return $ TokenBooleanLiteral False : rest
    | take 7 input == "boolean" = do
        rest <- tokenize (drop 7 input)
        return $ TokenTypeBoolean : rest
    | take 7 input == "integer" = do
        rest <- tokenize (drop 7 input)
        return $ TokenTypeInteger : rest
    | take 6 input == "return" = do
        rest <- tokenize (drop 6 input)
        return $ TokenReturn : rest
    | isDigit c = do
        let number = read (takeWhile isDigit input)
        rest <- tokenize (dropWhile isDigit input)
        return $ TokenIntegerLiteral number : rest
    | isAlpha c = do
        let name = takeWhile isAlpha input
        rest <- tokenize (dropWhile isAlpha input)
        return $ TokenSymbol name : rest
    | otherwise = Left ("Unexpected character " ++ [c] ++ "found during tokenizing")

parseProgram :: String -> Either String (Expression ())
parseProgram input = do
    tokens <- tokenize input
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
returnExpression = assignment <|> ifExpression <|> returnValue

assignment :: Parser (Expression ())
assignment = do
    name <- symbol
    varFunctionAssignment name <|> tupleDestructuring name

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
atom = try lambda <|> variableUsage <|> unaryMinus <|> integer <|> boolean <|> parenthesizedExpression

expressionCall :: Expression () -> Parser (Expression ())
expressionCall logicExpression = functionCall logicExpression <|> emptyExpressionCall logicExpression

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
    expressionCall base

emptyExpressionCall :: Expression () -> Parser (Expression ())
emptyExpressionCall expr = return expr

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
    expressionCall (Variable name ())

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
    expressionCall content

parseType :: Parser Type
parseType = chainr1 baseTypes functionType
    where baseTypes = booleanType <|> integerType <|> parenthesizedType
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
