module Main where

import ExprParser (satisfy, some, any, parseNumberToExpr, parseIndentToExpr, wordParser, expressionParser, znakParser, expressionParser, fullExpressionParser)
import Data.Char (isAlpha, isNumber)
import Parser (Parser(getParserFunc))
import Lib (eval)

import Data (Operator1 (..), Operator2 (..), Expr (..), Error (..))

import Data.Either (Either(Right))

main :: IO ()
-- main = print (getParserFunc parseNumberToExpr "12345.6789" >>= \(_, result) -> Right (Marg Neg result))
-- main = print (getParserFunc (wordParser "abcde") $ "abcdeabcde")
-- main = print (getParserFunc expressionParser $ "+ 5 7")
main = print (case (getParserFunc fullExpressionParser $ "   * 2 sqrt + 1 5") of 
    Left comment -> comment
    Right (suff, expression) -> show (eval (fromInteger <$> expression) []) )
