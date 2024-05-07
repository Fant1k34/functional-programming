{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Lib (evaluate, simplify, evaluateExpr, simplifyExpr, getListOfVar) where

import Data (Operator1 (..), Operator2 (..), Expr (..), Error (..))
import Data.List (lookup)

import Parser (Parser(..))
import ParserVariableInput (variablesListParser)
import ParserExpr (fullExpressionParser)

import Control.Monad (Monad(return))
import Control.Monad.State
import Data.Either (Either)

defineOperationEvaluator :: Floating a => Operator2 -> a -> a -> a
defineOperationEvaluator op
  | op == Div = (/)
  | op == Mul = (*)
  | op == Plus = (+)
  | op == Min = (-)
  | op == In = (**)


eval :: (Show a, Ord a, Floating a) => Expr a -> State [(String, a)] (Either (Error a) a)

eval (Arg value) = return (Right value)

eval (Var variable) = do
  env <- get
  return (case (Data.List.lookup variable env) of
    Just x -> Right x
    Nothing -> Left (VariableDoesNotExist variable))

eval (Marg Neg expr) = do
  env <- get
  result <- eval expr

  return (((-1)*) <$> result)


eval (Marg Sqrt expr) = do
  env <- get
  result <- eval expr

  return (case result of
    Left comment -> Left comment
    Right value -> if value >= 0 then Right (sqrt value) else Left (OutOfPossibleValuesError Sqrt value)
    )


eval (CE expr1 op expr2) = do
  env <- get
  result1 <- eval expr1
  result2 <- eval expr2

  return (case result1 of
    Right value1 -> case result2 of
      Right value2 -> case op of
        Div -> if value2 /= 0 then Right (defineOperationEvaluator op value1 value2)
        else Left (ZeroDivisionError value1)
        In -> if value1 == 0 && value2 < 0 then Left (IncorrectDegreeOfValue value2)
        else Right (defineOperationEvaluator op value1 value2)
        _ -> Right (defineOperationEvaluator op value1 value2)
      Left exception -> Left exception
    Left exception -> Left exception)


eval (Let var expr1 expr2) = do
  env <- get
  letBinding <- eval expr1

  case letBinding of
    Left comment -> return $ Left comment
    Right value -> (do 
        put (env ++ [(var, value)])
        eval expr2
        )



evaluate :: (Show a, Ord a, Floating a) => Expr a -> [(String, a)] -> Either (Error a) a
evaluate expr list = fst (runState (eval expr) list)


rule :: Eq a => Num a => Expr a -> Expr a
rule (Arg value) = Arg value

rule (Var variable) = Var variable

rule (Marg Neg (Marg Neg expr)) = expr
rule (Marg Neg (Arg 0)) = Arg 0
rule (Marg Sqrt (Arg 0)) = Arg 0
rule (Marg op expr) = Marg op expr

rule (CE (Arg 0) Mul expr2) = Arg 0
rule (CE expr1 Mul (Arg 0)) = Arg 0

rule (CE (Arg 1) Mul expr2) = expr2
rule (CE expr1 Mul (Arg 1)) = expr1

rule (CE (Arg 0) Plus expr2) = expr2
rule (CE expr1 Plus (Arg 0)) = expr1

rule (CE (Arg 0) Min expr2) = (Marg Neg expr2)
rule (CE expr1 Min (Arg 0)) = expr1

rule (CE expr1 Div (Arg 1)) = expr1
rule (CE expr1 Div (Marg Neg (Arg 1))) = Marg Neg expr1

rule (CE expr1 op expr2)
  | (expr1 == expr2) && (op == Min) = Arg 0
  | (expr1 == expr2) && (op == Div) = Arg 1
  | (expr1 == expr2) && (op == Plus) = CE (Arg 2) Mul expr1
  | (expr1 == expr2) && (op == Mul) = CE expr1 In (Arg 2)
  | otherwise = CE expr1 op expr2


simplify :: Eq a => Num a => Expr a -> Expr a
simplify (Arg value) = rule (Arg value)

simplify (Var variable) = rule (Var variable)

simplify (Marg anyOperator1 expr) = rule (Marg anyOperator1 (simplify expr))

simplify (CE expr1 op expr2) = rule (CE (simplify expr1) op (simplify expr2))


getListOfVar :: (Show a, Floating a) => String -> [(String, a)]
getListOfVar line = case getParserFunc variablesListParser line of
    Left comment -> error ("Error: " ++ comment)
    Right (_, varList) -> map (\(varName, value) -> (varName, fromInteger value)) varList



evaluateExpr :: (Ord a, Show a, Floating a) => String -> [(String, a)] -> String
evaluateExpr exprLine varList = case getParserFunc fullExpressionParser exprLine of
    Left comment -> comment
    Right (suff, expression) -> show (evaluate (fromInteger <$> expression) varList)


simplifyExpr :: String -> String
simplifyExpr exprLine = case getParserFunc fullExpressionParser exprLine of
    Left comment -> comment
    Right (suff, expression) -> show (simplify (fromInteger <$> expression))
