{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib where

import Data


beautyPrint' :: T -> String
beautyPrint' term = case term of
    Var x -> x
    Abstr args body -> "(\\" ++ (foldl (\p c -> p ++ c ++ " ") " " args) ++ "->" ++ beautyPrint body ++ ")"
    App [one] -> beautyPrint' one
    App terms -> "(" ++ (foldl (\p c -> p ++ (beautyPrint' c) ++ " ") " " terms) ++ ")"


beautyPrint :: T -> String
beautyPrint term = case term of
    Var x -> show x
    Abstr args body -> "\\" ++ (foldl (\p c -> p ++ c ++ " ") " " args) ++ "->" ++ beautyPrint' body
    App terms -> (foldl (\p c -> p ++ (beautyPrint' c) ++ " ") " " terms)
