module Main where

import Parser (Parser(..))
import ParserTerm

main :: IO ()
main = case (getParserFunc parseTerm "Kar kar") of
    Left comment -> putStrLn comment
    Right result -> putStrLn $ show result
