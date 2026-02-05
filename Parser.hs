{- HLINT ignore "Use <$>" -}

module Parser where

import AST
import Text.Parsec hiding (spaces)
import Text.Parsec.String (Parser)
import Prelude hiding (sum)

-- parse :: String -> Maybe Program

spaces :: Parser ()
spaces = skipMany (oneOf " \t")

number :: Parser Expression
number = do
    string <- many1 digit
    let n = read string
    return (Literal n)

symbol :: Parser Symbol
symbol = do
    name <- many1 letter
    return (Symbol name)

variable :: Parser Expression
variable = do
    name <- symbol
    return (Variable name)

term :: Parser Expression
term = number <|> variable

sum :: Parser Expression
sum = do
    left <- term
    spaces
    char '+'
    spaces
    right <- term
    return (Application (Application (Variable (Symbol "+")) left) right)

expression :: Parser Expression
expression = try functionApplication <|> try sum <|> try number <|> variable

variableAssignment :: Parser Assignment
variableAssignment = do
    name <- symbol
    spaces
    char '='
    spaces
    exp <- expression
    return (Assignment name exp)

functionDeclaration :: Parser Assignment
functionDeclaration = do
    name <- symbol
    char '('
    inputName <- symbol
    string "):"
    spaces
    exp <- expression
    return (Assignment name (Function inputName exp))

assignment :: Parser Assignment
assignment = try functionDeclaration <|> variableAssignment

functionApplication :: Parser Expression
functionApplication = do
    name <- symbol
    char '('
    exp <- expression
    char ')'
    return (Application (Variable name) exp)

parseProgram :: Parser (Program, Expression)
parseProgram = do
    assignments <- endBy (try assignment) newline
    exp <- expression
    newline
    return (assignments, exp)

parser :: String -> Either ParseError (Program, Expression)
parser = parse parseProgram ""
