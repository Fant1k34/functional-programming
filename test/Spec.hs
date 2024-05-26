import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ((@?=), testCase, assertBool, assertFailure)
import Data
import Lib
import Parser
import ParserTerm
import GHC.IO (unsafePerformIO)


makeReduction :: String -> String -> String
makeReduction term strategy =
  let reductionStrategy = if strategy == "EAGER" then eagerReduction else lazyReduction
  in
    case getParserFunc parseFullTerm term of
      Left comment -> "NO PARSE"
      Right (_, result) -> do
          let resultOfReduction = unsafePerformIO $ reductionStrategy result

          prettyPrint resultOfReduction


eagerReductionTest :: String -> String
eagerReductionTest term = makeReduction term "EAGER"


lazyReductionTest :: String -> String
lazyReductionTest term = makeReduction term "LAZY"


calculusGroups = testGroup "Lambda calculus" [ eagerReductionGroup, lazyReductionGroup ]
  where
    eagerReductionGroup = testGroup "eagerReductionGroup"
      [
        testCase "x" $ eagerReductionTest "x" @?= "x",
        testCase "x y" $ eagerReductionTest "x y" @?= "x y",
        testCase "x y z" $ eagerReductionTest "x y z" @?= "x y z",
        testCase "x (y z)" $ eagerReductionTest "x (y z)" @?= "x (y z)",
        testCase "(x) (y z)" $ eagerReductionTest "(x) (y z)" @?= "x (y z)",
        testCase "\\x . x x" $ eagerReductionTest "\\x . x x" @?= "\\x -> x x",
        testCase "(\\x . x) (y z)" $ eagerReductionTest "(\\x . x) (y z)" @?= "y z",
        testCase "(\\x . r) (y z)" $ eagerReductionTest "(\\x . r) (y z)" @?= "r",
        testCase "(\\x . x) (y z) d" $ eagerReductionTest "(\\x . x) (y z) d" @?= "y z d",
        testCase "(\\x . x) (\\y . y)" $ eagerReductionTest "(\\x . x) (\\y . y)" @?= "\\y -> y",
        testCase "(\\x . x) (\\x . x)" $ eagerReductionTest "(\\x . x) (\\x . x)" @?= "\\x -> x",
        testCase "(\\x . x) (\\x . x) (\\x . x)" $ eagerReductionTest "(\\x . x) (\\x . x) (\\x . x)" @?= "\\x -> x",
        testCase "(\\x . x) (\\y . y) (\\x . x)" $ eagerReductionTest "(\\x . x) (\\y . y) (\\x . x)" @?= "\\x -> x",
        testCase "(\\x . x) ((\\y . y) (\\x . x))" $ eagerReductionTest "(\\x . x) ((\\y . y) (\\x . x))" @?= "\\x -> x",
        testCase "(\\x . (\\y . x)) (\\y . y) (\\x . x)" $ eagerReductionTest "(\\x . (\\y . x)) (\\y . y) (\\x . x)" @?= "\\y -> y",
        testCase "(\\x . (\\y . y)) (\\y . y) (\\x . x)" $ eagerReductionTest "(\\x . (\\y . y)) (\\y . y) (\\x . x)" @?= "\\x -> x",
        testCase "long evaluation because of eager eval"
         $ eagerReductionTest ("(\\x . y) ((\\ a . " ++ foldl1 (\x y -> x ++ " " ++ y) (replicate 10000 "a") ++ ") (\\b . b))") @?= "y"
      ]
    lazyReductionGroup = testGroup "lazyReductionGroup"
      [
        testCase "x" $ lazyReductionTest "x" @?= "x",
        testCase "x y" $ lazyReductionTest "x y" @?= "x y",
        testCase "x y z" $ lazyReductionTest "x y z" @?= "x y z",
        testCase "x (y z)" $ lazyReductionTest "x (y z)" @?= "x (y z)",
        testCase "(x) (y z)" $ lazyReductionTest "(x) (y z)" @?= "x (y z)",
        testCase "\\x . x x" $ lazyReductionTest "\\x . x x" @?= "\\x -> x x",
        testCase "(\\x . x) (y z)" $ lazyReductionTest "(\\x . x) (y z)" @?= "y z",
        testCase "(\\x . r) (y z)" $ lazyReductionTest "(\\x . r) (y z)" @?= "r",
        testCase "(\\x . x) (y z) d" $ lazyReductionTest "(\\x . x) (y z) d" @?= "y z d",
        testCase "(\\x . x) (\\y . y)" $ lazyReductionTest "(\\x . x) (\\y . y)" @?= "\\y -> y",
        testCase "(\\x . x) (\\x . x)" $ lazyReductionTest "(\\x . x) (\\x . x)" @?= "\\x -> x",
        testCase "(\\x . x) (\\x . x) (\\x . x)" $ lazyReductionTest "(\\x . x) (\\x . x) (\\x . x)" @?= "\\x -> x",
        testCase "(\\x . x) (\\y . y) (\\x . x)" $ lazyReductionTest "(\\x . x) (\\y . y) (\\x . x)" @?= "\\x -> x",
        testCase "(\\x . x) ((\\y . y) (\\x . x))" $ lazyReductionTest "(\\x . x) ((\\y . y) (\\x . x))" @?= "\\x -> x",
        testCase "(\\x . (\\y . x)) (\\y . y) (\\x . x)" $ lazyReductionTest "(\\x . (\\y . x)) (\\y . y) (\\x . x)" @?= "\\y -> y",
        testCase "(\\x . (\\y . y)) (\\y . y) (\\x . x)" $ lazyReductionTest "(\\x . (\\y . y)) (\\y . y) (\\x . x)" @?= "\\x -> x",
        testCase "short evaluation because of lazy eval"
         $ lazyReductionTest ("(\\x . y) ((\\ a . " ++ foldl1 (\x y -> x ++ " " ++ y) (replicate 10000 "a") ++ ") (\\b . b))") @?= "y",
        
        testCase "K* (Omega Omega)" $ lazyReductionTest "(\\x . (\\y . y)) ((\\a . a a) (\\b . b b)) (\\c . c)" @?= "\\c -> c"
      ]


main :: IO ()
main = defaultMain $ testGroup "Tests" [ calculusGroups ]
