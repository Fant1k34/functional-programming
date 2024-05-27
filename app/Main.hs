module Main where

import Parser (Parser(..))
import ParserTerm
import Lib
import Lib (prettyPrint, lazyReduction, eagerReduction)

import Control.Monad.Trans.State.Lazy (State, evalState)


main :: IO ()
main = case getParserFunc parseFullTerm "(\\x . \\y . \\ z . k x y a b c d) (a b c y z)" of
    Left comment -> putStrLn comment
    Right (leftover, result) -> putStrLn $ prettyPrint $ evalState (eagerReduction result) 0
