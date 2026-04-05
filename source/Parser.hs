{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Eta reduce" -}
{- HLINT ignore "Avoid lambda" -}

module Parser (parseProgram) where

import AST
import Text.Parsec hiding (eof, char, newline, satisfy)
import Prelude hiding (sum)
import Data.Char (isAlpha, isDigit)

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
           | TokenGreater
           | TokenPlus
           | TokenMinus
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenTypeBoolean
           | TokenTypeInteger
           | TokenEOF
    deriving (Show, Eq)

tokenize :: String -> Either String [Token]
tokenize [] = Right [TokenEOF]
tokenize input@(c:cs)
    | c `elem` [' ', '\t', '\n', '\r'] = tokenize cs
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
    | c == '>' = do
        rest <- tokenize cs
        return $ TokenGreater : rest
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
    | isDigit c = do
        let number = read (takeWhile isDigit input)
        rest <- tokenize (dropWhile isDigit input)
        return $ TokenIntegerLiteral number : rest
    | isAlpha c = do
        let name = takeWhile isAlpha input
        rest <- tokenize (dropWhile isAlpha input)
        return $ TokenSymbol name : rest
    | otherwise = Left ("Unexpected character " ++ [c] ++ "found during tokenizing")

parseProgram :: String -> Either String Program
parseProgram input = do
    tokens <- tokenize input
    case parse program "" tokens of
        Left err -> Left (show err)
        Right p -> Right p

program :: Parser Program
program = do
    assignments <- many assignment
    out <- output
    match TokenEOF
    return (assignments, out)

assignment :: Parser Assignment
assignment = do
    name <- symbol
    content <- variableAssignment name <|> functionAssignment name
    match TokenSemicolon
    return content

output :: Parser Expression
output = do
    match TokenGreater
    exp <- expression
    match TokenSemicolon
    return exp

variableAssignment :: Symbol -> Parser Assignment
variableAssignment name = do
    match TokenAssign
    body <- expression
    return $ Assignment name body

functionAssignment :: Symbol -> Parser Assignment
functionAssignment name = do
    match TokenLeftParenthesis
    argument <- symbol
    match TokenColon
    argumentType <- parseType
    match TokenRightParenthesis
    match TokenColon
    returnType <- parseType
    match TokenArrow
    body <- expression
    return $ Assignment name (Function argument argumentType body returnType)

expression :: Parser Expression
expression = ifExpression <|> equality

ifExpression :: Parser Expression
ifExpression = do
    match TokenIf
    condition <- expression
    match TokenThen
    thenAtom <- expression
    match TokenElse
    elseAtom <- expression
    return (If condition thenAtom elseAtom)

equality :: Parser Expression
equality = do
    left <- additive
    rightMaybe <- optionMaybe (do
        match TokenEquality
        additive)
    return (case rightMaybe of
        Just right -> Application (Application (Variable (Symbol "==")) left) right
        Nothing -> left)

additive :: Parser Expression
additive = chainl1 atom additiveOperation
    where additiveOperation = do
            token <- match TokenPlus <|> match TokenMinus
            let operator = case token of
                    TokenPlus -> Symbol "+"
                    TokenMinus -> Symbol "-"
            return (\left right -> Application (Application (Variable operator) left) right)

atom :: Parser Expression
atom = atomSymbol <|> integer <|> boolean <|> parenthesizedExpression

-- This is used to parse both variable usages and function calls
atomSymbol :: Parser Expression
atomSymbol = do
    name <- symbol
    functionCall name <|> variableUsage name

functionCall :: Symbol ->  Parser Expression
functionCall symbol = do
    match TokenLeftParenthesis
    argument <- expression
    match TokenRightParenthesis
    return (Application (Variable symbol) argument)

variableUsage :: Symbol -> Parser Expression
variableUsage symbol = return (Variable symbol)

parenthesizedExpression :: Parser Expression
parenthesizedExpression = do
    match TokenLeftParenthesis
    content <- expression
    match TokenRightParenthesis
    return content

parseType :: Parser Type
parseType = booleanType <|> integerType <|> functionType

booleanType :: Parser Type
booleanType = do
    match TokenTypeBoolean
    return BooleanType

integerType :: Parser Type
integerType = do
    match TokenTypeInteger
    return IntegerType

functionType :: Parser Type
functionType = do
    match TokenLeftParenthesis
    argumentType <- parseType
    match TokenRightParenthesis
    returnType <- parseType
    return (FunctionType argumentType returnType)

symbol :: Parser Symbol
symbol = satisfy (\t -> case t of
    TokenSymbol name -> Just (Symbol name)
    _  -> Nothing)

integer :: Parser Expression
integer = satisfy (\t -> case t of
    TokenIntegerLiteral value -> Just (Integer value)
    _  -> Nothing)

boolean :: Parser Expression
boolean = satisfy (\t -> case t of
    TokenBooleanLiteral value -> Just (Boolean value)
    _  -> Nothing)

match :: Token -> Parser Token
match t = satisfy (\t' -> if t == t' then Just t else Nothing)

satisfy :: (Token -> Maybe a) -> Parser a
satisfy f = tokenPrim show (\pos _ _ -> pos) f
