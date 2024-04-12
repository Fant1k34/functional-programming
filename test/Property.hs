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

data LeafMode = 
  ZERO 
  | POSITIVE_VALUE
  | NEGATIVE_VALUE 
  | NOT_NEGATIVE_VALUE 
  | NOT_POSITIVE_VALUE 
  | ALL_POSSIBLE_VALUES 
  | VAR_X 
  | VAR_ONE_LETTER 
  | VAR_MANY_LETTERS 
  | NO_LIMITS


data OperationMode =
  ALL | DIVISION | NO_DIVISION | IN | NO_IN | SAFE | NO_SAFE


generateExprLeaf :: LeafMode -> Gen (Expr Integer)
generateExprLeaf leafMode = case leafMode of 
  ZERO -> return (Arg 0)
  POSITIVE_VALUE -> (do
    intValue <- Gen.integral_ (Range.constant 1 1000)

    return (Arg intValue)
    )
  NEGATIVE_VALUE -> (do
    intValue <- Gen.integral_ (Range.constant (-1000) (-1))

    return (Arg intValue)
    )
  NOT_NEGATIVE_VALUE -> (do
    intValue <- Gen.integral_ (Range.linear 0 1000)

    return (Arg intValue)
    )
  NOT_POSITIVE_VALUE -> (do
    intValue <- Gen.integral_ (Range.linearFrom 0 (-1000) 0)

    return (Arg intValue)
    )
  ALL_POSSIBLE_VALUES -> (do
    intValue <- Gen.integral_ (Range.constant (-1000) 1000)

    return (Arg intValue)
    )
  VAR_X -> return (Data.Var "x")
  VAR_ONE_LETTER -> return (Data.Var "a")
  VAR_MANY_LETTERS -> return (Data.Var "abc")
  NO_LIMITS -> (do
    randomValue <- Gen.integral_ (Range.constant 0 1)
    intValue <- Gen.integral_ (Range.constant (-10) 10)

    return (if randomValue == 0 then Arg intValue else Data.Var "x")
    )


generateExprMarg :: LeafMode -> Gen (Expr Integer)
generateExprMarg leafMode = do
  -- КОСЯК ТУТ В ТОМ, ЧТО НЕ ГЕНЕРИТСЯ ДЛИННЫЕ ЭКСПРЕШШЕНЫ ТУТ!!!
  expr <- generateExprLeaf leafMode

  return (Marg Sqrt expr)


generateRandomOperation :: OperationMode -> Gen Operator2
generateRandomOperation opMode = case opMode of 
  ALL -> (do
    randomValue <- Gen.integral_ (Range.constant 0 4)

    return (case randomValue of
      0 -> Plus
      1 -> Min
      2 -> Div
      3 -> Mul
      _ -> In
      )
    )
  DIVISION -> return Div
  NO_DIVISION -> (do
    randomValue <- Gen.integral_ (Range.constant 0 3)

    return (case randomValue of
      0 -> Plus
      1 -> Min
      3 -> Mul
      _ -> In
      )
    )
  IN -> return In
  NO_IN -> (do
    randomValue <- Gen.integral_ (Range.constant 0 3)

    return (case randomValue of
      0 -> Plus
      1 -> Min
      3 -> Mul
      _ -> Div
      )
    )
  SAFE -> (do
    randomValue <- Gen.integral_ (Range.constant 0 2)

    return (case randomValue of
      0 -> Plus
      1 -> Min
      _ -> Mul
      )
    )
  NO_SAFE ->(do
    randomValue <- Gen.integral_ (Range.constant 0 1)

    return (case randomValue of
      0 -> Div
      _ -> In
      )
    )



generateExprSample :: Integer -> LeafMode -> OperationMode -> Gen (Expr Integer)
generateExprSample limit leafMode opMode
  | limit <= 0 = generateExprLeaf leafMode
  | otherwise = do
    indexOfExpression <- Gen.integral_ (Range.constant 0 9)

    case indexOfExpression of
      0 -> generateExprMarg leafMode
      _ -> (do
        randomOperation <- generateRandomOperation opMode
        expr1 <- generateExprSample (limit - 1) leafMode opMode
        expr2 <- generateExprSample (limit - 1) leafMode opMode

        return (CE expr1 randomOperation expr2) 
        )


propCorrectness :: Property
propCorrectness = property (do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse (reverse xs) === xs
    )


propEvalExpressionWithSqrtError :: Property
propEvalExpressionWithSqrtError = property (do
  expr0 <- forAll $ generateExprSample 7 NEGATIVE_VALUE SAFE

  case evaluate (fmap fromInteger expr0) [] of
    Left (OutOfPossibleValuesError _ _) -> assert True
    value -> show False === (show value ++ " got from " ++ show expr0)
  )


propEvalExpressionWithZeroDivisionError :: Property
propEvalExpressionWithZeroDivisionError = property (do
  expr <- forAll $ generateExprSample 19 NOT_NEGATIVE_VALUE NO_SAFE

  case evaluate (fmap fromInteger expr) [] of
    Left (ZeroDivisionError _) -> assert True
    value -> show False === (show value ++ " got from " ++ show expr)
  )


props :: [TestTree]
props = [ testProperty "Check evaluation of expression with Sqrt error" propEvalExpressionWithSqrtError,
          testProperty "Check evaluation of expression with Zero division" propEvalExpressionWithZeroDivisionError ]