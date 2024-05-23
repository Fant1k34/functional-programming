import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ((@?=), testCase, assertBool, assertFailure, Assertion)

import Data (E(..), X(..), F (Fun), S (ExprAsS), Op2(..))
import Eval (evalFunc)

import Control.Monad.Trans.State.Lazy (evalStateT)
import Control.Monad.Trans.IO (runIOT)
import Parser (Parser(getParserFunc))
import ParserFunc (parserProgram)


launchParser :: [Char] -> F Float -> Assertion
launchParser sourceCode correctValue = do
    let ast = getParserFunc (parserProgram "main" []) sourceCode

    case ast of
        Left comment -> fail $ "Parsing Error: " ++ comment
        Right (_, value) -> value @?= correctValue


mainGroup :: TestTree
mainGroup = testGroup "Parser" [ fromDecimalGroup ]
  where
    fromDecimalGroup = testGroup "Expressions"
      [
        testCase "1.0" $ launchParser "1.0" (Fun "main" [] [] (ExprAsS (Number 1))),
        testCase "  0.15" $ launchParser "  0.15" (Fun "main" [] [] (ExprAsS (Number 0.15))),
        testCase "  0.15  " $ launchParser "  0.15  " (Fun "main" [] [] (ExprAsS (Number 0.15))),
        testCase "0.15  " $ launchParser "0.15  " (Fun "main" [] [] (ExprAsS (Number 0.15))),
        testCase "0.15\t " $ launchParser "0.15\t " (Fun "main" [] [] (ExprAsS (Number 0.15))),
        testCase "31.02*78984.15" $ launchParser "31.02*78984.15" (Fun "main" [] [] (ExprAsS (CE (Number 31.02) Mult (Number 78984.15)))),
        testCase "2.0 + 95.84" $ launchParser "2.0 + 95.84" (Fun "main" [] [] (ExprAsS (CE (Number 2) Plus (Number 95.84)))),
        testCase "2.0 - 95.84" $ launchParser "2.0 - 95.84" (Fun "main" [] [] (ExprAsS (CE (Number 2) Min (Number 95.84)))),
        testCase "2.0 * 95.84" $ launchParser "2.0 * 95.84" (Fun "main" [] [] (ExprAsS (CE (Number 2) Mult (Number 95.84)))),
        testCase "2.0 / 95.84" $ launchParser "2.0 / 95.84" (Fun "main" [] [] (ExprAsS (CE (Number 2) Del (Number 95.84)))),
        testCase "x1 + y1 - 55.04" $ launchParser "x1 + y1 - 55.04" (Fun "main" [] [] (ExprAsS $ CE (CE (VarAsExpr (Var "x1")) Plus (VarAsExpr (Var "y1"))) Min (Number 55.04))),
        testCase "x1 - y1 - 55.04" $ launchParser "x1 - y1 - 55.04" (Fun "main" [] [] (ExprAsS $ CE (CE (VarAsExpr (Var "x1")) Min (VarAsExpr (Var "y1"))) Min (Number 55.04))),
        testCase "\"Example of String\"" $ launchParser "\"Example of String\"" (Fun "main" [] [] (ExprAsS (Str "Example of String"))),
        testCase "\"Example of String\" + \"!\" + \"??\"" $ launchParser "\"Example of String\" + \"!\" + \"??\""
         (Fun "main" [] [] (ExprAsS $ CE (CE (Str "Example of String") Plus (Str "!")) Plus (Str "??"))),
        testCase "True" $ launchParser "True" (Fun "main" [] [] (ExprAsS (Boolean True))),
        testCase "False" $ launchParser "False" (Fun "main" [] [] (ExprAsS (Boolean False))),
        testCase "False || False" $ launchParser "False || False" (Fun "main" [] [] (ExprAsS (Boolean False))),
        testCase "False || True" $ launchParser "False || True" (Fun "main" [] [] (ExprAsS (Boolean True))),
        testCase "True || False" $ launchParser "True || False" (Fun "main" [] [] (ExprAsS (Boolean True))),
        testCase "True || True" $ launchParser "True || True" (Fun "main" [] [] (ExprAsS $ CE (Boolean True) Or (Boolean True))),
        testCase "False && False" $ launchParser "False && False" (Fun "main" [] [] (ExprAsS $ CE (Boolean False) And (Boolean False))),
        testCase "False && True" $ launchParser "False && True" (Fun "main" [] [] (ExprAsS (Boolean False))),
        testCase "True && False" $ launchParser "True && False" (Fun "main" [] [] (ExprAsS (Boolean False))),
        testCase "True && True" $ launchParser "True && True" (Fun "main" [] [] (ExprAsS (Boolean True))),
        testCase "True && False || False && True && False || True || False" $
         launchParser "True && False || False && True && False || True || False"
          (Fun "main" [] [] (ExprAsS $ CE (CE (Boolean False) And (Boolean False)) Or (Boolean True))),
        testCase "True && someExpression" $ launchParser "True && someExpression" (Fun "main" [] [] (ExprAsS (VarAsExpr (Var "someExpression")))),
        testCase "True || someExpression" $ launchParser "True || someExpression" (Fun "main" [] [] (ExprAsS $ CE (Boolean True) Or (VarAsExpr (Var "someExpression")))),
        testCase "False && someExpression" $ launchParser "False && someExpression" (Fun "main" [] [] (ExprAsS $ CE (Boolean False) And (VarAsExpr (Var "someExpression")))),
        testCase "False || someExpression" $ launchParser "False || someExpression" (Fun "main" [] [] (ExprAsS (VarAsExpr (Var "someExpression")))),
        testCase "1.0 == 2.0" $ launchParser "1.0 == 2.0" (Fun "main" [] [] (ExprAsS $ CE (Number 1) Eql (Number 2))),
        testCase "1.0 != 2.0" $ launchParser "1.0 != 2.0" (Fun "main" [] [] (ExprAsS $ CE (Number 1) NotEql (Number 2))),
        testCase "1.0 < 2.0" $ launchParser "1.0 < 2.0" (Fun "main" [] [] (ExprAsS $ CE (Number 1) Less (Number 2))),
        testCase "1.0 <= 2.0" $ launchParser "1.0 <= 2.0" (Fun "main" [] [] (ExprAsS $ CE (Number 1) LessEql (Number 2))),
        testCase "1.0 > 2.0" $ launchParser "1.0 > 2.0" (Fun "main" [] [] (ExprAsS $ CE (Number 1) More (Number 2))),
        testCase "1.0 >= 2.0" $ launchParser "1.0 >= 2.0" (Fun "main" [] [] (ExprAsS $ CE (Number 1) MoreEql (Number 2))),
        testCase "1.0 + x < x * x" $ launchParser "1.0 + x < x * x" 
         (Fun "main" [] [] (ExprAsS $ CE (CE (Number 1) Plus (VarAsExpr (Var "x"))) Less (CE (VarAsExpr (Var "x")) Mult (VarAsExpr (Var "x"))))),
        testCase "a < b || b >= c && d == 3" $ launchParser "a < b || b >= c && d == 3"
         (Fun "main" [] [] (ExprAsS $ CE (CE (VarAsExpr (Var "a")) Less (VarAsExpr (Var "b"))) Or
          (CE (CE (VarAsExpr (Var "b")) MoreEql (VarAsExpr (Var "c"))) And (CE (VarAsExpr (Var "d")) Eql (Number 3)))  ))
      ]


main :: IO ()
main = defaultMain $ testGroup "Tests" [ mainGroup ]