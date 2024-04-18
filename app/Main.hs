module Main where

import Data.Char (isAlpha, isNumber)
import Parser (Parser(getParserFunc))
import Lib (simplify, evaluateExpr, simplifyExpr, getListOfVar)

import Data (Operator1 (..), Operator2 (..), Expr (..), Error (..))

import Data.Either (Either(Right))


main :: IO ()
main = do
    putStrLn "Welcome to autho calculator with variables. Here is some rules of this calculator:"
    putStrLn "1. No negative numbers. Sorry ;("
    putStrLn "2. Only one unary operator: sqrt"
    putStrLn "3. Supported binary operators: +, -, *, /"
    putStrLn "4. All calculations could be done only in prefix form"
    putStrLn "5. Variables could be used without any extra symbols \""
    putStrLn "6. Variables contains first alphabet symbol, then any combination of numbers and alphabet symbols"
    putStrLn "7. When the line is entered you would be asked for mode: simplify expression or evaluate expression"
    putStrLn "Example of input: + 2 sqrt * + var1 1 var2"
    putStrLn "This input is equalent to: 2 + sqrt((var1 + 1) * var2)"
    putStrLn "Write your expression:"

    exprLine <- getLine

    putStrLn "Chose the mode of calculator:"
    putStrLn "Write \"1\" for evaluation mode"
    putStrLn "Write \"2\" for simplify mode"

    mode <- getLine

    if (read mode :: Int) == 1 then (do
        putStrLn "Write variables in following way: variable1 = value, variable2 = value, ..., variableN = value"
        putStrLn "Input example: var1 = 12, var2 = 6"
        putStrLn "If your line does not contain any variables then press Enter"

        varLine <- getLine

        let result = evaluateExpr exprLine (getListOfVar varLine)

        putStrLn result
        ) else print (simplifyExpr exprLine)
