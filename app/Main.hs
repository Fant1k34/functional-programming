module Main where

import Parser (Parser(..))
import ParserTerm
import Lib
import Lib (prettyPrint, lazyReduction, eagerReduction)

-- (\\x . \\y . x) (\\a . a a) (\\b . b b)
main :: IO ()
main = case getParserFunc parseFullTerm "(\\x . \\ z . y x) (a b z)" of
    Left comment -> putStrLn comment
    Right (leftover, result) -> putStrLn $ prettyPrint $ lazyReduction result
