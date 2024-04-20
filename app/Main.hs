module Main where

import Parser (Parser(..))
import ParserTerm

main :: IO ()
main = case (getParserFunc parseFullTerm "(l x . (l h u . (l r . u)) x) x (y (z (k)))") of
    Left comment -> putStrLn comment
    Right result -> putStrLn $ show result
