module Main where

import Parser (Parser(..))
import ParserTerm
import Lib
import Lib (prettyPrint, lazyReduction, eagerReduction)

-- (\\x . \\y . x) (\\a . a a) (\\b . b b)
-- (\\x . \\y . y) ((\\a . a a) (\\b . b b)) (\\c . c)
main :: IO ()
main = case getParserFunc parseFullTerm "(\\x . y) ((\\a . a a) (\\b . b b))" of
    Left comment -> putStrLn comment
    Right (leftover, result) -> do
        resultOfReduction <- eagerReduction result

        putStrLn $ prettyPrint resultOfReduction
