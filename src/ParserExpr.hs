{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use join" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# HLINT ignore "Alternative law, right identity" #-}
module ParserExpr where

import Parser (Parser(..))
import ParserCore

import Utils (razryad, concatNumbers, castCharToInt)

import Data.Char (isAlpha, isAlphaNum, isNumber, isSeparator)
import Control.Applicative (Alternative((<|>), empty))

import Data (Operator1 (..), Operator2 (..), Expr (..), Error (..))
import Data.Foldable1 (foldlM1)


defineActionByZnak :: Char -> Operator2
defineActionByZnak znak
    | znak == '-' = Min
    | znak == '+' = Plus
    | znak == '*' = Mul
    | znak == '/' = Div
    | znak == '^' = In
    | otherwise = error "Inner Error: Something was broken during parsing. defineActionByZnak got not action"


parseNumberToExpr :: Integral a => Parser (Expr a)
parseNumberToExpr = do
    numberList <- some (satisfy isNumber castCharToInt) (++)
    let result = Prelude.foldl1 concatNumbers numberList

    return (Arg result)

-- Заготовка для парсинга abc.de чисел. Но пока не сходятся типы Integral и Fractional -> Num
-- (do
--     part1List <- many (satisfy isNumber castCharToInt) (++)
--     let part1 = Prelude.foldl1 concatNumbers part1List

--     satisfy (=='.') (\x -> x)

--     part2List <- many (satisfy isNumber castCharToInt) (++)
--     let part2 = Prelude.foldl1 concatNumbers part2List

--     let result = part1 + part2 / (10 ^ (razryad part2))
--     return (Arg result)
-- )


parseIndentToExpr :: Parser (Expr Integer)
parseIndentToExpr = do
    var <- parseIndet

    return (Var var)



znakParser :: Parser Operator2
znakParser = satisfy (\char -> elem char ['+', '-', '/', '*', '^']) defineActionByZnak


unaryParser :: Parser Operator1
unaryParser = do
    wordParser "sqrt"

    return Sqrt

unaryOperator :: Parser (Expr Integer)
unaryOperator = do
    operation <- unaryParser
    separatorParser
    value <- expressionParser

    return (Marg operation value)


binaryOperator :: Parser (Expr Integer)
binaryOperator = do
    znak <- znakParser
    separatorParser
    value1 <- expressionParser
    separatorParser
    value2 <- expressionParser

    return (CE value1 znak value2)


expressionParser :: Parser (Expr Integer)
expressionParser = binaryOperator <|> unaryOperator <|> parseIndentToExpr <|> parseNumberToExpr


fullExpressionParser :: Parser (Expr Integer)
fullExpressionParser = do
    possibleSeparatorParser
    result <- expressionParser
    possibleSeparatorParser
    isFullyApplied

    return result
