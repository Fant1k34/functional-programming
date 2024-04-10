module ParserCore (satisfy, some, ParserCore.any, isFullyApplied, parseNumber, parseIndet, wordParser, separatorParser, possibleSeparatorParser, parserWithSeparator) where


import Parser (Parser(..))

import Utils

import Data.Char (isAlpha, isAlphaNum, isNumber, isSeparator)
import Control.Applicative (Alternative((<|>), empty))


-- Функция принимает условие для первого символа строки внутри парсера
-- и функцию для преобразования символа в нужную струкутуру
-- Возвращает парсер, который при удачном применении (если
-- условие на первом элементе выполняется) возвращает структуру, полученную через трансформацию
satisfy :: (Char -> Bool) -> (Char -> a) -> Parser a
satisfy cond castFunction = Parser (\input ->
    case input of
        (h : t) -> if cond h then Right (t, castFunction h) else Left ("Parse Error: Predicate is not followed on " ++ show h ++ " in input " ++ input)
        _ -> Left "Parse Error: Empty input"
        )

-- Функция принимает Парсер, и функцию склейки результатов применения парсеров
-- Возвращает парсер, который применяет 1 или более раз парсер и возвращает склеенную структуру
some :: Parser a -> (a -> a -> a) -> Parser a
some p1 concatResults = (p1 >>= (\successResultP1 ->
    Parser ( \input -> case getParserFunc (some p1 concatResults) input of
        Left comment -> Right (input, successResultP1)
        Right (suff', value) -> Right (suff', concatResults successResultP1 value) ) ) ) <|> empty


-- Функция принимает Парсер, функцию склейки результатов применения парсеров и значение по умолчанию
-- Возвращает парсер, который применяет 0 или более раз парсер и возвращает склеенную структуру
-- Если парсер применён 0 раз, то возвращает значение по умолчанию
any :: Parser a -> (a -> a -> a) -> a -> Parser a
any p1 concatResults baseCase = some p1 concatResults <|> return baseCase


-- Функция, которая возвращает парсер, который говорит о том, осталось ли часть строки неотпарсенной
isFullyApplied :: Parser Bool
isFullyApplied = Parser (\input -> if (length input == 0) then Right (input, True) else Left ("Parse Error: String is not fully parsed -- " ++ input))


-- Функция парсинга целых цифр
parseNumber :: Parser Integer
parseNumber = do
    numberList <- some (satisfy isNumber castCharToInt) (++)
    let result = Prelude.foldl1 concatNumbers numberList

    return result


-- Функция парсинга имен переменных
parseIndet :: Parser [Char]
parseIndet = do
    letter <- satisfy isAlpha (: [])
    other <- ParserCore.any (satisfy isAlphaNum (: [])) (++) ""
    return (letter ++ other)



-- Функция принимает строку и возвращает парсер, который парсит строку
wordParser :: String -> Parser String
wordParser word = if (length word == 0) then return "" else foldl1 (\p1 p2 -> p1 >>= (\successP1 -> Parser (\input -> 
    case (getParserFunc p2 input) of 
        Left comment -> Left comment
        Right (suff, value) -> Right (suff, successP1 ++ value)))) (map (\char -> (satisfy (==char) (: []))) word)


-- Парсер разделителей
separatorParser :: Parser String
separatorParser = some (satisfy isSeparator (: [])) (++)


-- Парсер возможных разделителей
possibleSeparatorParser :: Parser String
possibleSeparatorParser = ParserCore.any separatorParser (++) ""


parserWithSeparator :: Parser a -> String -> Parser [a]
parserWithSeparator p sep = (do
    value <- p
    wordParser sep
    results <- parserWithSeparator p sep

    return ([value] ++ results)
    ) <|> (do
    value <- p

    return [value]
    )
