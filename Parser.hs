{- HLINT ignore "Use <$>" -}

module Parser (parseProgram) where

import AST
import Text.Parsec hiding (eof, char, newline)
import Prelude hiding (sum)
import Data.Char (isAlpha, isDigit)

type Parser = Parsec [Token] ()

data Token = TokenSymbol String
           | TokenIntegerLiteral Int
           | TokenBooleanLiteral Bool
           | TokenChar Char
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenParser
           | TokenNewline
           | TokenEOF
    deriving Show

tokenize :: String -> Either String [Token]
tokenize [] = Right [TokenEOF]
tokenize input@(c:cs)
    | c `elem` [' ', '\r', '\t'] = tokenize cs
    | c `elem` ['(', ')', '+', '='] = do
        rest <- tokenize cs
        return $ TokenChar c : rest
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
    | isAlpha c = do
        rest <- tokenize (dropWhile isAlpha input)
        return $ TokenSymbol (takeWhile isAlpha input) : rest
    | isDigit c = do
        rest <- tokenize (dropWhile isDigit input)
        return $ TokenIntegerLiteral (read (takeWhile isDigit input)) : rest
    | c == '\n' = do
        rest <- tokenize cs
        return $ TokenNewline : rest
    | otherwise = Left $ "Unexpected character: " ++ [c]

parseProgram :: String -> Either String Program
parseProgram input = do
    tokens <- tokenize input
    case parse program "" tokens of
        Left err -> Left (show err)
        Right p -> Right p

program :: Parser Program
program = do
    assignments <- many assignment
    exp <- expression
    newline
    eof
    return (assignments, exp)

assignment :: Parser Assignment
assignment = do
    a <- try functionDeclaration <|> try variableDeclaration
    newline
    return a

expression :: Parser Expression
expression = ifExpression <|> try functionApplication <|> try sum <|> try number <|> try boolean <|> try variableUsage

ifExpression :: Parser Expression
ifExpression = do
    ifToken
    condition <- expression
    thenToken
    a <- expression
    elseToken
    b <- expression
    return (If condition a b)

functionDeclaration :: Parser Assignment
functionDeclaration = do
    name <- symbol
    char '('
    inputName <- symbol
    char ')'
    char '='
    exp <- expression
    return (Assignment name (Function inputName exp))

variableDeclaration :: Parser Assignment
variableDeclaration = do
    name <- symbol
    char '='
    exp <- expression
    return (Assignment name exp)

functionApplication :: Parser Expression
functionApplication = do
    name <- symbol
    char '('
    exp <- expression
    char ')'
    return (Application (Variable name) exp)

sum :: Parser Expression
sum = do
    left <- term
    char '+'
    right <- term
    return (Application (Application (Variable (Symbol "+")) left) right)

variableUsage :: Parser Expression
variableUsage = do
    name <- symbol
    return (Variable name)

term :: Parser Expression
term = number <|> variableUsage


advance pos _ _ = incSourceColumn pos 1

symbol :: Parser Symbol
symbol = tokenPrim show advance $ \t -> case t of
    TokenSymbol s -> Just (Symbol s)
    _             -> Nothing

number :: Parser Expression
number = tokenPrim show advance $ \t -> case t of
    TokenIntegerLiteral n -> Just (IntegerLiteral n)
    _              -> Nothing

boolean :: Parser Expression
boolean = tokenPrim show advance $ \t -> case t of
    TokenBooleanLiteral b -> Just (BooleanLiteral b)
    _                    -> Nothing

char :: Char -> Parser ()
char c = tokenPrim show advance $ \t -> case t of
    TokenChar ch | ch == c -> Just ()
    _                      -> Nothing

ifToken :: Parser ()
ifToken = tokenPrim show advance $ \t -> case t of
    TokenIf -> Just ()
    _       -> Nothing

thenToken :: Parser ()
thenToken = tokenPrim show advance $ \t -> case t of
    TokenThen -> Just ()
    _         -> Nothing

elseToken :: Parser ()
elseToken = tokenPrim show advance $ \t -> case t of
    TokenElse -> Just ()
    _         -> Nothing

newline :: Parser ()
newline = tokenPrim show advance $ \t -> case t of
    TokenNewline -> Just ()
    _            -> Nothing

eof :: Parser ()
eof = tokenPrim show advance $ \t -> case t of
    TokenEOF -> Just ()
    _        -> Nothing
