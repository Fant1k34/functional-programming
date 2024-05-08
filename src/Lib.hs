{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Lib (evaluate, evalC, evaluateExpr, getListOfVar) where

import Data (Operator1 (..), Operator2 (..), Expr (..), Error (..), CodeStr(..))
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


evalE :: (Show a, Ord a, Floating a) => Expr a -> State [(String, a)] (Either (Error a) a)
evalE (Arg value) = return (Right value)
evalE (Var variable) = do
  env <- get
  return (case (Data.List.lookup variable env) of
    Just x -> Right x
    Nothing -> Left (VariableDoesNotExist variable))

evalE (Marg Neg expr) = do
  env <- get
  result <- evalE expr

  return (((-1)*) <$> result)

evalE (Marg Sqrt expr) = do
  env <- get
  result <- evalE expr

  return (case result of
    Left comment -> Left comment
    Right value -> if value >= 0 then Right (sqrt value) else Left (OutOfPossibleValuesError Sqrt value)
    )

evalE (CE expr1 op expr2) = do
  env <- get
  result1 <- evalE expr1
  result2 <- evalE expr2

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


evalC :: (Show a, Ord a, Floating a) => CodeStr a -> State [(String, a)] (Either (Error a) a)
evalC (Let var expr1 expr2) = do
  env <- get
  letBinding <- evalC expr1

  case letBinding of
    Left comment -> return $ Left comment
    Right value -> (do 
        put (env ++ [(var, value)])
        evalC expr2
        )

evalC (Expression expr) = do
  env <- get
  evalE expr


evaluate :: (Show a, Ord a, Floating a) => CodeStr a -> [(String, a)] -> Either (Error a) a
evaluate code list = fst (runState (evalC code) list)


getListOfVar :: (Show a, Floating a) => String -> [(String, a)]
getListOfVar line = case getParserFunc variablesListParser line of
    Left comment -> error ("Error: " ++ comment)
    Right (_, varList) -> map (\(varName, value) -> (varName, fromInteger value)) varList


evaluateExpr :: (Ord a, Show a, Floating a) => String -> [(String, a)] -> String
evaluateExpr exprLine varList = case getParserFunc fullExpressionParser exprLine of
    Left comment -> comment
    Right (suff, expression) -> show (evaluate (Expression $ fromInteger <$> expression) varList)
