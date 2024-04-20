module ParserTerm where

import ParserCore
import Parser (Parser(..))
import Control.Applicative (Alternative((<|>), empty))

import Data.Char (isAlpha, isAlphaNum, isNumber, isSeparator)
import Data


parseVar :: Parser T
parseVar = do
    varName <- some (satisfy isAlpha)

    return (Var varName)


parseAbstraction :: Parser T
parseAbstraction = do
    possibleSeparatorParser
    satisfy (=='l')
    possibleSeparatorParser
    varList <- parserWithSeparator (some (satisfy isAlpha)) " "
    possibleSeparatorParser

    satisfy (=='.')
    possibleSeparatorParser
    term <- parseTerm

    return (Abstr varList term)


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
    terms <- parserWithSeparator (parseAbstraction <|> parseSkobBlock <|> parseVar) " "

    return (App terms)


parseFullTerm :: Parser T
parseFullTerm = do
    result <- parseTerm
    isFullyApplied

    return result
