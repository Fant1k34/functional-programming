module Main where

import Parser (Parser(..))
import ParserTerm
import Lib
import Lib (prettyPrint)


main :: IO ()
main = case (getParserFunc parseFullTerm "(S (\\ x . M (N L)) \\ x . ((M)) (N L))") of
    Left comment -> putStrLn comment
    Right (leftover, result) -> putStrLn $ prettyPrint result
