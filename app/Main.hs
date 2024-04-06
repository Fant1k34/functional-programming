module Main where

import ExprParser (satisfy, some, many, parseNumberToExpr, parseIndentToExpr, keywordParser)
import Data.Char (isAlpha, isNumber)
import Parser (Parser(getParserFunc))

import Data (Operator1 (..), Operator2 (..), Expr (..), Error (..))

import Data.Either (Either(Right))

main :: IO ()
-- main = print (getParserFunc parseNumberToExpr "12345.6789" >>= \(_, result) -> Right (Marg Neg result))
main = print (getParserFunc (keywordParser "abcde") $ "abcdeabcde")
