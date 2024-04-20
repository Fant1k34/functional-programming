module Main where

import Parser (Parser(..))
import ParserTerm
import Lib


main :: IO ()
main = case (getParserFunc parseFullTerm "(l x . (l h u . (l r . u)) x) x (y (z (k)))") of
    Left comment -> putStrLn comment
    Right (leftover, result) -> putStrLn $ beautyPrint result

