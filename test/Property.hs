{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Property where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data
import Lib

import Test.Tasty
import Test.Tasty.Hedgehog


generateExprLeaf :: Gen (Expr Int)
generateExprLeaf = do
  randomValue <- Gen.integral_ (Range.constant 0 1)

  if randomValue == 0 then (do
    value <- Gen.integral_ (Range.constant (-10) 10)

    return (Arg value)
    ) else (do
      return (Data.Var "x")
      )

generateExprMarg :: Gen (Expr Int)
generateExprMarg = do
  expr <- generateExprLeaf

  return (Marg Sqrt expr)


generateRandomOperation :: Gen Operator2
generateRandomOperation = do
  randomValue <- Gen.integral_ (Range.constant 0 4)

  return (case randomValue of
    0 -> Plus
    1 -> Min
    2 -> Div
    3 -> Mul
    _ -> In
    )


generateExprSample :: Int -> Gen (Expr Int)
generateExprSample limit
  | limit <= 0 = generateExprLeaf
  | otherwise = do
    indexOfExpression <- Gen.integral_ (Range.constant 0 9)

    case indexOfExpression of
      0 -> generateExprLeaf
      1 -> generateExprMarg
      _ -> (do
        randomOperation <- generateRandomOperation
        expr1 <- generateExprSample (limit - 1)
        expr2 <- generateExprSample (limit - 1)

        return (CE expr1 randomOperation expr2) 
        )


    


propCorrectness :: Property
propCorrectness = property (do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse (reverse xs) === xs
    )

propSimpleExpression :: Property
propSimpleExpression = property (do
  expr0 <- forAll $ generateExprSample 3
  expr1 <- forAll $ generateExprSample 1
  expr2 <- forAll $ generateExprSample 2

  show expr0 === "50"

  )


props :: [TestTree]
props = [ testProperty "Simple check" propSimpleExpression ]