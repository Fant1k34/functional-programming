{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module ParserTerm where

import ParserCore
import Parser (Parser(..))
import Control.Applicative (Alternative((<|>), empty))

import Data.Char (isAlpha, isAlphaNum, isNumber, isSeparator)
import Data


parseVar :: Parser V
parseVar = do
    possibleSeparatorParser
    varName <- some (satisfy isAlpha)

    return (Var varName)


parseAbstraction :: Parser T
parseAbstraction = do
    possibleSeparatorParser
    satisfy (=='\\')
    possibleSeparatorParser
    var <- parseVar
    possibleSeparatorParser

    satisfy (=='.')
    possibleSeparatorParser
    term <- parseTerm

    return (Abstr var term)


-- parseApplication :: Parser T
-- parseApplication = do
--     possibleSeparatorParser
--     terms <- parserWithSeparator parseTerm " "

--     return (App terms)

parseSkobBlock :: Parser T
parseSkobBlock = parseInBrackets parseTerm "(" ")"


-- Парсер это последовательность из:
-- - Абстракций
-- - Переменных
-- - Скобочных блоков, в которых находится терм
parseTerm :: Parser T
parseTerm = do
    terms <- parserWithSeparator (parseAbstraction <|> parseSkobBlock <|> VarToT <$> parseVar) (some (satisfy (\el -> el == ' ' || el == '\t')))

    return $ foldl1 App terms


parseFullTerm :: Parser T
parseFullTerm = do
    result <- parseTerm
    isFullyApplied

    return result
