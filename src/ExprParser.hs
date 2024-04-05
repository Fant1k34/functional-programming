{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use join" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use camelCase" #-}
module ExprParser where

import Parser (Parser(..))
import Data.Char (isAlpha, isAlphaNum, isNumber)
import Control.Applicative (Alternative((<|>), empty))

import Data (Operator1 (..), Operator2 (..), Expr (..), Error (..))


castCharToInt :: Integral a => Char -> [a]
castCharToInt char
    | char == '0' = [0]
    | char == '1' = [1]
    | char == '2' = [2]
    | char == '3' = [3]
    | char == '4' = [4]
    | char == '5' = [5]
    | char == '6' = [6]
    | char == '7' = [7]
    | char == '8' = [8]
    | char == '9' = [9]
    | otherwise = error "Inner Error: Something was broken"


razryad :: Integral a => a -> Int -> Int
razryad value a = if value < 10 then 1 else razryad (div value 10) (a) + 1


concatNumbers :: Integral a => a -> a -> a
concatNumbers x y = x * (10 ^ (razryad y 0)) + y


satisfy :: (Char -> Bool) -> (Char -> a) -> Parser a
satisfy cond castFunction = Parser (\input ->
    case input of
        (h : t) -> if cond h then Right (t, castFunction h) else Left ("Parse Error: Predicate is not followed on " ++ show h ++ " in input " ++ input)
        _ -> Left "Parse Error: Empty input"
        )


some :: Parser a -> (a -> a -> a) -> Parser a
some p_1 concat_results = (p_1 >>= (\success_result_p_1 ->
    Parser ( \input -> case getParserFunc (some p_1 concat_results) input of
        Left comment -> Right (input, success_result_p_1)
        Right (suff', value) -> Right (suff', concat_results success_result_p_1 value) ) ) ) <|> empty


many :: Parser a -> (a -> a -> a) -> Parser a
many p_1 concat_results = do
    r <- p_1
    s <- some p_1 concat_results
    return (concat_results r s)


isFullyApplied :: Parser Bool
isFullyApplied = Parser (\input -> if (length input == 0) then Right (input, True) else Left ("Parse Error: String is not fully parsed -- " ++ input))


parseNumber :: Integral a => Parser (Expr a)
parseNumber = do
    numberList <- many (satisfy isNumber castCharToInt) (++)
    let result = Prelude.foldl1 concatNumbers numberList

    return (Arg result)


-- parseIndet :: Parser [Char]
-- parseIndet = do
--     letter <- satisfy isAlpha (: [])
--     other <- some (satisfy isAlphaNum (: [])) (++)
--     return (letter ++ other)




-- parseIndet :: Parser String
-- parseIndet = do
--         h <- satisfy isAlpha
--         t <- orParser input
--         h : t
        -- where 
        --     orParser = Parser (\input -> do
        --         h <- getParserFunc ((satisfy isAlpha) <|> (satisfy isNumber)) input
        --         t <- getParserFunc orParser input
        --         )
        --         <|> return []

