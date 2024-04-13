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


propCorrectness :: Property
propCorrectness = property (do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    xs === xs
    )


propEvalExpressionWithSqrtError :: Property
propEvalExpressionWithSqrtError = property (do
  expr0 <- forAll $ generateExprSample 13 NEGATIVE_VALUE SAFE True

  case evaluate (fmap fromInteger expr0) [] of
    Left (OutOfPossibleValuesError _ _) -> assert True
    value -> show False === show value ++ " got from " ++ show expr0
  )


propEvalExpressionWithZeroDivisionError :: Property
propEvalExpressionWithZeroDivisionError = property (do
  expr <- forAll $ generateExprSample 5 ZERO DIVISION True

  case evaluate (fmap fromInteger expr) [] of
    Left (ZeroDivisionError _) -> assert True
    value -> show False === show value ++ " got from " ++ show expr
  )



propEvalExpressionWithInError :: Property
propEvalExpressionWithInError = property (do
  exprPos <- forAll $ generateExprSample 7 ALL_POSSIBLE_VALUES SAFE False
  exprAnySafe <- forAll $ generateExprSample 7 ALL_POSSIBLE_VALUES SAFE False

  let expr = CE (CE exprAnySafe Mul (Arg 0)) In (Marg Neg (CE exprPos In 2))

  case evaluate (fmap fromInteger expr) [] of
    Left (IncorrectDegreeOfValue _) -> assert True
    value -> show False === show value ++ " got from " ++ show expr
  )


propEvalExpressionWithUndefinedVar :: Property
propEvalExpressionWithUndefinedVar = property (do
  exprVar <- forAll $ generateExprSample 13 NO_LIMITS SAFE False
  exprAnySafe <- forAll $ generateExprSample 7 ALL_POSSIBLE_VALUES SAFE False
  randomOperation <- forAll $ generateRandomOperation SAFE

  let expr = CE exprAnySafe randomOperation exprVar

  case evaluate (fmap fromInteger expr) [] of
    Left _ -> assert True
    value -> show False === show value ++ " got from " ++ show expr
  )


propEvalExpressionWithVar :: Property
propEvalExpressionWithVar = property (do
  exprVar <- forAll $ generateExprSample 3 VAR_X SAFE False
  exprAnySafe <- forAll $ generateExprSample 7 ALL_POSSIBLE_VALUES SAFE False
  randomOperation <- forAll $ generateRandomOperation SAFE

  let expr = CE exprAnySafe randomOperation exprVar

  case evaluate (fmap fromInteger expr) [("x", 123)] of
    Left comment -> show False === show comment ++ " got from " ++ show expr
    _ -> assert True
  )


propEvalExpressionDiv :: Property
propEvalExpressionDiv = property (do
  subExpr1 <- forAll $ generateExprSample 4 ALL_POSSIBLE_VALUES SAFE False
  subExpr2' <- forAll $ generateExprSample 3 ALL_POSSIBLE_VALUES SAFE False

  -- x ^ 2 + 1 > 0
  let subExpr2 = CE (CE subExpr2' In 2) Plus (Arg 1)

  let expr = CE subExpr1 Div subExpr2

  -- Check that subExpr1 / subExpr2 == expr
  case evaluate (fmap fromInteger expr) [] of
    Left comment -> show False === show comment ++ " got from " ++ show expr
    Right resutOfExpr ->
      case evaluate (fmap fromInteger subExpr1) [] of
        Left comment -> show False === show comment ++ " got from " ++ show subExpr1
        Right subValue1 ->
          case evaluate (fmap fromInteger subExpr2) [] of
            Left comment -> show False === show comment ++ " got from " ++ show subExpr2
            Right subValue2 -> resutOfExpr === subValue1 / subValue2
  )


propEvalExpressionIn :: Property
propEvalExpressionIn = property (do
  subExpr1 <- forAll $ generateExprSample 7 POSITIVE_VALUE SAFE False
  subExpr2' <- forAll $ generateExprSample 7 NOT_NEGATIVE_VALUE SAFE False

  -- x ^ 2 > 0
  let subExpr2 = CE subExpr2' In 2

  let expr = CE subExpr1 In subExpr2

  -- Check that subExpr1 ^ subExpr2 == expr
  case evaluate (fmap fromInteger expr) [] of
    Left comment -> show False === show comment ++ " got from " ++ show expr
    Right resutOfExpr ->
      case evaluate (fmap fromInteger subExpr1) [] of
        Left comment -> show False === show comment ++ " got from " ++ show subExpr1
        Right subValue1 ->
          case evaluate (fmap fromInteger subExpr2) [] of
            Left comment -> show False === show comment ++ " got from " ++ show subExpr2
            Right subValue2 -> resutOfExpr === subValue1 ** subValue2
  )


propEvalExpressionSafe :: Operator2 -> (Float -> Float -> Float) -> Property
propEvalExpressionSafe opExpr op = property (do
  subExpr1 <- forAll $ generateExprSample 3 ALL_POSSIBLE_VALUES SAFE False
  subExpr2 <- forAll $ generateExprSample 3 ALL_POSSIBLE_VALUES SAFE False

  let expr = CE subExpr1 opExpr subExpr2

  case evaluate (fmap fromInteger expr) [] of
    Left comment -> show False === show comment ++ " got from " ++ show expr
    Right resutOfExpr ->
      case evaluate (fmap fromInteger subExpr1) [] of
        Left comment -> show False === show comment ++ " got from " ++ show subExpr1
        Right subValue1 ->
          case evaluate (fmap fromInteger subExpr2) [] of
            Left comment -> show False === show comment ++ " got from " ++ show subExpr2
            Right subValue2 -> resutOfExpr === op subValue1 subValue2
  )


props :: [TestTree]
props = [ testProperty "Check evaluation of expression with Sqrt error" propEvalExpressionWithSqrtError,
          testProperty "Check evaluation of expression with Zero division" propEvalExpressionWithZeroDivisionError,
          testProperty "Check evaluation of expression with 0 ^ (- ...)" propEvalExpressionWithInError,
          testProperty "Check evaluation of expression with undefined variable" propEvalExpressionWithUndefinedVar,
          testProperty "Check evaluation of correct expression with defined variable" propEvalExpressionWithVar,
          testProperty "Check evaluation of E1 / E2" propEvalExpressionDiv,
          testProperty "Check evaluation of E1 ^ E2" propEvalExpressionIn,
          testProperty "Check evaluation of E1 + E2" (propEvalExpressionSafe Plus (+)),
          testProperty "Check evaluation of E1 - E2" (propEvalExpressionSafe Min (-)),
          testProperty "Check evaluation of E1 * E2" (propEvalExpressionSafe Mul (*)) ]
