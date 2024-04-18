import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ((@?=), testCase, assertBool, assertFailure)
import Data
import Lib


evalGroups = testGroup "Eval" [ numbersTestGroup, binaryEvaluationsGroup, complexExpressionGroup, variablesGroup ]
  where
    numbersTestGroup = testGroup "Numbers"
      [
        testCase "Double as expression == Double" $ evaluate (Arg 56) [] @?= Right 56,
        testCase "Negative of expression == Negative Double" $ evaluate (Marg Neg (Arg 3)) [] @?= Right (-3),
        testCase "Negative of 0 as expression == 0" $ evaluate (Marg Neg (Arg 0)) [] @?= Right 0,
        testCase "Negative of negative expression = Positive Double" $ evaluate (Marg Neg (Marg Neg (Arg 3))) [] @?= Right 3,
        testCase "Sqrt of 4 == 2" $ evaluate (Marg Sqrt (Arg 4)) [] @?= Right 2,
        testCase "Sqrt of 169 == 13" $ evaluate (Marg Sqrt (Arg (169))) [] @?= Right 13,
        testCase "Sqrt of 0 == 0" $ evaluate (Marg Sqrt (Arg 0)) [] @?= Right 0,
        testCase "Sqrt of -169 = OutOfPossibleValuesError" $
          evaluate (Marg Sqrt (Arg (-169))) [] @?= Left (OutOfPossibleValuesError Sqrt (-169)),
        testCase "Sqrt of negative expression" $
          evaluate (Marg Sqrt (Marg Neg (Arg 169))) [] @?= Left (OutOfPossibleValuesError Sqrt (-169))
      ]
    binaryEvaluationsGroup = testGroup "Binary evalutation"
      [
        testCase "CE (Arg 1) Plus (Arg 2.2) == 3.2" $ evaluate (CE (Arg 1) Plus (Arg 2.2)) [] @?= Right 3.2,
        testCase "CE (Arg 2) Min (Arg 1.25) == 0.75" $ evaluate (CE (Arg 2) Min (Arg 1.25)) [] @?= Right 0.75,
        testCase "CE (Arg 4) Div (Arg 2) == 3.2" $ evaluate (CE (Arg 4) Div (Arg 2)) [] @?= Right 2,
        testCase "CE (Arg 5) Div (Arg 2) == 2.5" $ evaluate (CE (Arg 5) Div (Arg 2)) [] @?= Right 2.5,
        testCase "CE (Arg (-5)) Div (Arg 2) == -2.5" $ evaluate (CE (Arg (-5)) Div (Arg 2)) [] @?= Right (-2.5),
        testCase "CE (Arg (-5)) Div (Marg Neg (Arg 2)) == 2.5" $ evaluate (CE (Arg (-5)) Div (Marg Neg (Arg 2))) [] @?= Right 2.5,
        testCase "CE (Arg 0) Div (Arg 2) == 0" $ evaluate (CE (Arg 0) Div (Arg 2)) [] @?= Right 0,
        testCase "CE (Arg 2) In (Arg 3) == 8" $ evaluate (CE (Arg 2) In (Arg 3)) [] @?= Right 8,
        testCase "CE (Arg 0) In (Arg 2) == 0" $ evaluate (CE (Arg 0) In (Arg 2)) [] @?= Right 0,
        testCase "CE (Arg 5) In (Arg 0) == 1" $ evaluate (CE (Arg 5) In (Arg 0)) [] @?= Right 1,
        testCase "CE (Arg 0) In (Arg 0) == 1" $ evaluate (CE (Arg 0) In (Arg 0)) [] @?= Right 1,
        testCase "CE (Arg 6) Div (Arg 0) == ZeroDivisionError 6" $ evaluate (CE (Arg 6) Div (Arg 0)) [] @?= Left (ZeroDivisionError 6),
        testCase "CE (Arg 0) Div (Arg 0) == ZeroDivisionError 0" $ evaluate (CE (Arg 0) Div (Arg 0)) [] @?= Left (ZeroDivisionError 0),
        testCase "CE (Arg 0) In (Arg (-5)) == IncorrectDegreeOfValue (-5)" $ evaluate (CE (Arg 0) In (Arg (-5))) [] @?= Left (IncorrectDegreeOfValue (-5))
      ]
    complexExpressionGroup = testGroup "Complex expressions" [
        testCase "Complex expressions 1" $ evaluate (CE (CE (Arg 5) Min (Arg 6)) Plus (Arg 4)) [] @?= Right 3,
        testCase "Complex expressions 2" $ evaluate (CE (Marg Neg (Arg 7)) Min (Marg Neg (Arg 3))) [] @?= Right (-4),
        testCase "Complex expressions 3" $ evaluate (CE (CE (Arg 2) Mul (Marg Sqrt (Arg 9))) Plus (CE (Arg 0) Plus (Marg Neg (Arg 5)))) [] @?= Right 1,
        testCase "Complex expressions with error 1" $ evaluate (CE (CE (Arg 2) Mul (Marg Sqrt (Arg (-9)))) Plus (CE (Arg 0) Plus (Marg Neg (Arg 5)))) [] @?= Left (OutOfPossibleValuesError Sqrt (-9)),
        testCase "Complex expressions with error 2" $ evaluate (CE (CE (Arg 2) Mul (Marg Sqrt (Arg (9)))) Plus (CE (Arg 2) Div (Marg Neg (Arg 0)))) [] @?= Left (ZeroDivisionError 2),
        testCase "Complex expressions with error 3" $ evaluate (CE (CE (Arg 2) Mul (Marg Sqrt (Arg (-9)))) Plus (CE (Arg 2) Div (Marg Neg (Arg 0)))) [] @?= Left (OutOfPossibleValuesError Sqrt (-9))
      ]
    variablesGroup = testGroup "Variables" [
        testCase "x = 2" $ evaluate (Var "x") [("x", 2)] @?= Right 2,
        testCase "y = 0" $ evaluate (Var "y") [("y", 0)] @?= Right 0,
        testCase "x * 3 = 6" $ evaluate (CE (Var "x") Mul (Arg 3)) [("x", 2)] @?= Right 6,
        testCase "x * y = 0" $ evaluate (CE (Var "x") Mul (Var "y")) [("x", 2), ("y", 0)] @?= Right 0,
        testCase "x + y = 2" $ evaluate (CE (Var "x") Plus (Var "y")) [("x", 2), ("y", 0)] @?= Right 2,
        testCase "x ^ y = 1" $ evaluate (CE (Var "x") In (Var "y")) [("x", 2), ("y", 0)] @?= Right 1,
        testCase "y ^ (-2) = IncorrectDegreeOfValue (-2)" $ evaluate (CE (Var "y") In (Arg (-2)))  [("x", 2), ("y", 0)] @?= Left (IncorrectDegreeOfValue (-2)),
        testCase "z * 3 = VariableDoesNotExist z" $ evaluate (CE (Var "z") Mul (Arg 3)) [("x", 2), ("y", 0)] @?= Left (VariableDoesNotExist "z"),
        testCase "3 * z = VariableDoesNotExist z" $ evaluate (CE (Arg 3) Mul (Var "z")) [("x", 2), ("y", 0)] @?= Left (VariableDoesNotExist "z")
      ]

simplifyGroups = testGroup "Simplify" [ simplifyGroup ]
  where
    simplifyGroup = testGroup "Rules"
      [
        testCase "x = x" $ simplify (Var "x") @?= Var "x",
        testCase "x + 0 = x" $ simplify (CE (Var "x") Plus (Arg 0)) @?= Var "x",
        testCase "0 + x = x" $ simplify (CE (Arg 0) Plus (Var "x")) @?= Var "x",
        testCase "x * 1 = x" $ simplify (CE (Var "x") Mul (Arg 1)) @?= Var "x",
        testCase "1 * x = x" $ simplify (CE (Arg 1) Mul (Var "x")) @?= Var "x",
        testCase "x - 0 = x" $ simplify (CE (Var "x") Min (Arg 0)) @?= Var "x",
        testCase "0 - x = -x" $ simplify (CE (Arg 0) Min (Var "x")) @?= Marg Neg (Var "x"),
        testCase "x * 0 = 0" $ simplify (CE (Var "x") Mul (Arg 0)) @?= Arg 0,
        testCase "0 * x = 0" $ simplify (CE (Arg 0) Mul (Var "x")) @?= Arg 0,
        testCase "x / 1 = x" $ simplify (CE (Var "x") Div (Arg 1)) @?= Var "x"
      ]

parseGroups = testGroup "Parse" [ parseGroup, parseWithVars ]
  where
    parseGroup = testGroup "Numbers only"
      [
        testCase "+ 2 4" $ evaluateExpr "+ 2 4" [] @?= "Right 6.0",
        testCase "* sqrt 16 sqrt sqrt 16" $ evaluateExpr "* sqrt 16 sqrt sqrt 16" [] @?= "Right 8.0",
        testCase "- 2 4" $ evaluateExpr "- 2 4" [] @?= "Right (-2.0)",
        testCase "/ 4 2" $ evaluateExpr "/ 4 2" [] @?= "Right 2.0",
        testCase "/ 2 4" $ evaluateExpr "/ 2 4" [] @?= "Right 0.5",
        testCase "^ 2 4" $ evaluateExpr "^ 2 4" [] @?= "Right 16.0",
        testCase "* - / 5 2 1 + ^ 2 3 4" $ evaluateExpr "* - / 5 2 1 + ^ 2 3 4" [] @?= "Right 18.0"
      ]
    parseWithVars = testGroup "With variables"
      [
        testCase "+ x 4" $ evaluateExpr "+ x 4" [("x", 2)] @?= "Right 6.0",
        testCase "* sqrt var1 sqrt sqrt var2" $ evaluateExpr "* sqrt var1 sqrt sqrt var2" [("var1", 4), ("var2", 16)] @?= "Right 4.0",
        testCase "* - x1 1 + ^ x2 x3 x4" $ evaluateExpr "* - x1 2 + ^ x2 x3 x4" [("x1", 1), ("x2", 2), ("x3", 3), ("x4", 4)] @?= "Right (-12.0)",
        testCase "* - x1 1 + ^ x2 x3 x4" $ evaluateExpr "* - x1 2 + ^ x2 x3 x4" [("x1", 1), ("x2", 2), ("x3", 3)] @?= "Left VariableDoesNotExist: variable x4 is not defined"
      ]


main :: IO ()
main = defaultMain $ testGroup "Tests" [ evalGroups, simplifyGroups, parseGroups ]
