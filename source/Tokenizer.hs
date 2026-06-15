{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Eta reduce" -}
{- HLINT ignore "Avoid lambda" -}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Tokenizer (Token(..), tokenize) where

import AST
import Text.Parsec hiding (eof, char, newline, satisfy)
import Data.Char (isAlpha, isDigit)
import Data.Map (insert, empty, fromList)

type Parser = Parsec [Token] ()

data Token = TokenIntegerLiteral Int
           | TokenBooleanLiteral Bool
           | TokenSymbol String
           | TokenLeftParenthesis
           | TokenRightParenthesis
           | TokenLeftCurlyBrace
           | TokenRightCurlyBrace
           | TokenComma
           | TokenSemicolon
           | TokenColon
           | TokenPeriod
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
    | c == '{' = do
        rest <- tokenize cs
        return $ TokenLeftCurlyBrace : rest
    | c == '}' = do
        rest <- tokenize cs
        return $ TokenRightCurlyBrace : rest
    | c == ',' = do
        rest <- tokenize cs
        return $ TokenComma : rest
    | c == ';' = do
        rest <- tokenize cs
        return $ TokenSemicolon : rest
    | c == ':' = do
        rest <- tokenize cs
        return $ TokenColon : rest
    | c == '.' = do
        rest <- tokenize cs
        return $ TokenPeriod : rest
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
