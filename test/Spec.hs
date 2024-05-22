import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ((@?=), testCase, assertBool, assertFailure)
import Data
import Data (toDecimal)

mainGroup = testGroup "Main" [ fromDecimalGroup, toDecimalGroup, operations ]
  where
    fromDecimalGroup = testGroup "fromDecimalGroup"
      [
        testCase "233_10 = 11101001_2" $ fromDecimal 233 2 @?= Number 11101001 2,
        testCase "5_10 = 101_2" $ fromDecimal 5 2 @?= Number 101 2,
        testCase "3575_10 = 4812_9" $ fromDecimal 3575 9 @?= Number 4812 9
      ]
    toDecimalGroup = testGroup "toDecimalGroup" [
        testCase "11101001_2 = 233_10" $ toDecimal (Number 11101001 2) @?= 233,
        testCase "101_2 = 5_10" $ toDecimal (Number 101 2) @?= 5,
        testCase "4812_9 = 3575_10" $ toDecimal (Number 4812 9) @?= 3575
      ]
    operations = testGroup "operations" [
        testCase "11101001_2 + 5_10" $ Number 11101001 2 + Number 101 2 @?= Number 11101110 2,
        testCase "11101001_2 * 1" $ Number 11101001 2 * Number 1 2 @?= Number 11101001 2,
        testCase "11101001_2 * 0" $ Number 11101001 2 * Number 0 2 @?= Number 0 2,
        testCase "11101001_2 < 11101010_2" $ Number 11101001 2 < Number 11101010 2 @?= True,
        testCase "11101011_2 < 11101010_2" $ Number 11101011 2 < Number 11101010 2 @?= False,
        testCase "11101001_2 <= 11101010_2" $ Number 11101001 2 <= Number 11101010 2 @?= True,
        testCase "11101011_2 <= 11101010_2" $ Number 11101011 2 <= Number 11101010 2 @?= False,
        testCase "11101001_2 <= 11101001_2" $ Number 11101001 2 <= Number 11101001 2 @?= True,
        testCase "11101001_2 > 11101010_2" $ Number 11101001 2 > Number 11101010 2 @?= False,
        testCase "11101011_2 > 11101010_2" $ Number 11101011 2 > Number 11101010 2 @?= True,
        testCase "11101001_2 >= 11101010_2" $ Number 11101001 2 >= Number 11101010 2 @?= False,
        testCase "11101011_2 >= 11101010_2" $ Number 11101011 2 >= Number 11101010 2 @?= True,
        testCase "11101001_2 >= 11101001_2" $ Number 11101001 2 >= Number 11101001 2 @?= True
      ]


main :: IO ()
main = defaultMain $ testGroup "Tests" [ mainGroup ]