{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use join" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use camelCase" #-}
module ExprParser where

import Parser (Parser(..))
import Data.Char (isAlpha, isAlphaNum, isNumber, isSeparator)
import Control.Applicative (Alternative((<|>), empty))

import Data (Operator1 (..), Operator2 (..), Expr (..), Error (..))
import Data.Foldable1 (foldlM1)


castCharToInt :: Num a => Char -> [a]
castCharToInt char
    | char == '0' = [0]
    | char == '1' = [1]
    | char == '2' = [2]
    | char == '3' = [3]
    | char == '4' = [4]
    | char == '5' = [5]
    | char == '6' = [6]
    | char == '7' = [7]
    | char == '8' = [8]
    | char == '9' = [9]
    | otherwise = error "Inner Error: Something was broken"


defineActionByZnak :: Char -> Operator2
defineActionByZnak znak
    | znak == '-' = Min
    | znak == '+' = Plus
    | znak == '*' = Mul
    | znak == '/' = Div
    | otherwise = error "Inner Error: Something was broken during parsing. defineActionByZnak got not action"


razryad :: Integral a => a -> Int -> Int
razryad value a = if value < 10 then 1 else razryad (div value 10) (a) + 1


concatNumbers :: Integral a => a -> a -> a
concatNumbers x y = x * (10 ^ (razryad y 0)) + y


satisfy :: (Char -> Bool) -> (Char -> a) -> Parser a
satisfy cond castFunction = Parser (\input ->
    case input of
        (h : t) -> if cond h then Right (t, castFunction h) else Left ("Parse Error: Predicate is not followed on " ++ show h ++ " in input " ++ input)
        _ -> Left "Parse Error: Empty input"
        )


some :: Parser a -> (a -> a -> a) -> Parser a
some p1 concatResults = (p1 >>= (\successResultP1 ->
    Parser ( \input -> case getParserFunc (some p1 concatResults) input of
        Left comment -> Right (input, successResultP1)
        Right (suff', value) -> Right (suff', concatResults successResultP1 value) ) ) ) <|> empty


any :: Parser a -> (a -> a -> a) -> a -> Parser a
any p1 concatResults baseCase = some p1 concatResults <|> return baseCase


isFullyApplied :: Parser Bool
isFullyApplied = Parser (\input -> if (length input == 0) then Right (input, True) else Left ("Parse Error: String is not fully parsed -- " ++ input))


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


parseIndet :: Parser [Char]
parseIndet = do
    letter <- satisfy isAlpha (: [])
    other <- some (satisfy isAlphaNum (: [])) (++)
    return (letter ++ other)


parseIndentToExpr :: Parser (Expr Int)
parseIndentToExpr = do
    var <- parseIndet

    return (Var var)



wordParser :: String -> Parser String
wordParser word = if (length word == 0) then return "" else foldl1 (\p1 p2 -> p1 >>= (\successP1 -> Parser (\input -> 
    case (getParserFunc p2 input) of 
        Left comment -> Left comment
        Right (suff, value) -> Right (suff, successP1 ++ value)))) (map (\char -> (satisfy (==char) (: []))) word)


separatorParser :: Parser String
separatorParser = some (satisfy isSeparator (: [])) (++)


znakParser :: Parser Operator2
znakParser = satisfy (\char -> elem char ['+', '-', '/', '*']) defineActionByZnak


unaryOperator :: Parser (Expr Int)
unaryOperator = do
    wordParser "sqrt"
    separatorParser
    expressionParser


binaryOperator :: Parser (Expr Int)
binaryOperator = do
    znak <- znakParser
    separatorParser
    value1 <- expressionParser
    separatorParser
    value2 <- expressionParser

    return (CE value1 znak value2)


expressionParser :: Parser (Expr Int)
expressionParser = binaryOperator <|> unaryOperator <|> parseIndentToExpr <|> parseNumberToExpr



