module ParserVariableInput where

import Parser(Parser(..))
import ParserCore

import Control.Applicative (Alternative((<|>), empty))

variableEnterParser :: Parser (String, Integer)
variableEnterParser = do
    possibleSeparatorParser
    var <- parseIndet
    possibleSeparatorParser
    satisfy (== '=')
    possibleSeparatorParser
    value <- parseNumber
    possibleSeparatorParser

    return (var, value)


variablesListParser :: Parser [(String, Integer)]
-- variablesListParser = Parser (\input -> case getParserFunc variableEnterParser input of 
--     Left comment -> Left comment
--     Right (suff, value) -> case getParserFunc variablesListParser suff of
--         Left _ -> Right (suff, value)
--         Right (suff', new_value) -> Right (suff', value ++ new_value)) <|> empty
variablesListParser = parserWithSeparator variableEnterParser ","
