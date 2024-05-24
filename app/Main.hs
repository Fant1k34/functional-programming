module Main where

import Parser (Parser(..))
import ParserTerm
import Lib
import Lib (prettyPrint)


main :: IO ()
main = case getParserFunc parseFullTerm "(\\x . \\y . x) (\\a . a a) (\\b . b b)" of
    Left comment -> putStrLn comment
    Right (leftover, result) -> putStrLn $ prettyPrint result
