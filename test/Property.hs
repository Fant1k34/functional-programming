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
import Hedgehog.Internal.Show (Value(Integer))


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
    intValue <- Gen.integral_ (Range.exponential 0 1000)

    return (Arg intValue)
    )
  NOT_POSITIVE_VALUE -> (do
    intValue <- Gen.integral_ (Range.exponentialFrom 0 (-1000) 0)

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


generateExprMarg :: Integer -> LeafMode -> OperationMode -> Gen (Expr Integer)
generateExprMarg limit leafMode opMode = do
  expr <- generateExprSample (limit - 1) leafMode opMode True

  return (Marg Sqrt expr)


generateRandomOperation :: OperationMode -> Gen Operator2
generateRandomOperation opMode = case opMode of
  ALL -> Gen.element [Plus, Min, Div, Mul, In]
  DIVISION -> return Div
  NO_DIVISION -> Gen.element [Plus, Min, Mul, In]
  IN -> return In
  NO_IN -> Gen.element [Plus, Min, Div, Mul]
  SAFE -> Gen.element [Plus, Min, Mul]
  NO_SAFE -> Gen.element [Div, In]


generateExprSample :: Integer -> LeafMode -> OperationMode -> Bool -> Gen (Expr Integer)
generateExprSample limit leafMode opMode isSqrtAllowed
  | limit <= 0 = generateExprLeaf leafMode
  | otherwise = do
    indexOfExpression <- if isSqrtAllowed then Gen.integral_ (Range.constant 0 9) else Gen.integral_ (Range.constant 1 9)

    case indexOfExpression of
      0 -> generateExprMarg limit leafMode opMode
      _ -> (do
        randomOperation <- generateRandomOperation opMode
        expr1 <- generateExprSample (limit - 1) leafMode opMode isSqrtAllowed
        expr2 <- generateExprSample (limit - 1) leafMode opMode isSqrtAllowed

        return (CE expr1 randomOperation expr2)
        )


propEvalExpressionWithSqrtError :: Property
propEvalExpressionWithSqrtError = property (do
  expr0 <- forAll $ generateExprSample 13 NEGATIVE_VALUE SAFE True

  case evaluate (fmap fromInteger expr0) [] of
    Left (OutOfPossibleValuesError _ _) -> assert True
    _ -> failure
  )


propEvalExpressionWithZeroDivisionError :: Property
propEvalExpressionWithZeroDivisionError = property (do
  expr <- forAll $ generateExprSample 5 ZERO DIVISION True

  case evaluate (fmap fromInteger expr) [] of
    Left (ZeroDivisionError _) -> assert True
    value -> failure
  )


propEvalExpressionWithInError :: Property
propEvalExpressionWithInError = property (do
  exprPos <- forAll $ generateExprSample 7 ALL_POSSIBLE_VALUES SAFE False
  exprAnySafe <- forAll $ generateExprSample 7 ALL_POSSIBLE_VALUES SAFE False

  let expr = CE (CE exprAnySafe Mul (Arg 0)) In (Marg Neg (CE exprPos In 2))

  case evaluate (fmap fromInteger expr) [] of
    Left (IncorrectDegreeOfValue _) -> assert True
    value -> failure
  )


propEvalExpressionWithUndefinedVar :: Property
propEvalExpressionWithUndefinedVar = property (do
  exprVar <- forAll $ generateExprSample 13 NO_LIMITS SAFE False
  exprAnySafe <- forAll $ generateExprSample 7 ALL_POSSIBLE_VALUES SAFE False
  randomOperation <- forAll $ generateRandomOperation SAFE

  let expr = CE exprAnySafe randomOperation exprVar

  case evaluate (fmap fromInteger expr) [] of
    Left _ -> assert True
    value -> failure
  )


propEvalExpressionWithVar :: Property
propEvalExpressionWithVar = property (do
  exprVar <- forAll $ generateExprSample 3 VAR_X SAFE False
  exprAnySafe <- forAll $ generateExprSample 7 ALL_POSSIBLE_VALUES SAFE False
  randomOperation <- forAll $ generateRandomOperation SAFE

  let expr = CE exprAnySafe randomOperation exprVar

  case evaluate (fmap fromInteger expr) [("x", 123)] of
    Left comment -> failure
    _ -> assert True
  )


propEvalExpression :: Operator2 -> (Expr Integer -> Expr Integer) -> (Float -> Float -> Float) -> Property
propEvalExpression operator exprWrapper evaluator = property (do
  subExpr1 <- forAll $ generateExprSample 3 POSITIVE_VALUE SAFE False
  subExpr2' <- forAll $ generateExprSample 3 NOT_NEGATIVE_VALUE SAFE False

  let subExpr2 = exprWrapper subExpr2'
  let expr = CE subExpr1 operator subExpr2

  -- Check that subExpr1 (^/*+-) subExpr2 == expr
  case evaluate (fmap fromInteger expr) [] of
    Left comment -> failure
    Right resutOfExpr ->
      case evaluate (fmap fromInteger subExpr1) [] of
        Left comment -> failure
        Right subValue1 ->
          case evaluate (fmap fromInteger subExpr2) [] of
            Left comment -> failure
            Right subValue2 -> resutOfExpr === evaluator subValue1 subValue2
  )


props :: [TestTree]
props = [ testProperty "Check evaluation of expression with Sqrt error" propEvalExpressionWithSqrtError,
          testProperty "Check evaluation of expression with Zero division" propEvalExpressionWithZeroDivisionError,
          testProperty "Check evaluation of expression with 0 ^ (- ...)" propEvalExpressionWithInError,
          testProperty "Check evaluation of expression with undefined variable" propEvalExpressionWithUndefinedVar,
          testProperty "Check evaluation of correct expression with defined variable" propEvalExpressionWithVar,
          testProperty "Check evaluation of E1 / E2" (propEvalExpression Div (\expr -> CE (CE expr In 2) Plus (Arg 1)) (/)),
          testProperty "Check evaluation of E1 ^ E2" (propEvalExpression In (\expr -> CE (CE expr In 2) Plus (Arg 1)) (**)),
          testProperty "Check evaluation of E1 + E2" (propEvalExpression Plus id (+)),
          testProperty "Check evaluation of E1 - E2" (propEvalExpression Min id (-)),
          testProperty "Check evaluation of E1 * E2" (propEvalExpression Mul id (*)) ]
