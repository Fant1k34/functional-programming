{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib where

import Data

-- prettyPrint:
-- переменная: x (или y) -> без скобок
-- Абстракция: \x . T -> без скобок
-- Аппликация: T T -> возможны скобки, рассмотрим ниже

    -- T           T
    -- x           y       -> без скобок
    -- x           \x . T  -> без скобок
    -- x           (T T)   -> со скобками справа
    -- (\x . T)    y       -> со скобками слева

    -- T1 T2       y       -> без скобок
    -- T1 (\x . T) y       -> скобки

    -- T1 T2       \x . T  -> без скобок
    -- T1 (\x . T) \x . T  -> скобки

    -- T1 T2      (T1' T2')-> скобки
    -- T1 (\x . T)(T1' T2')-> скобки


prettyPrint :: T -> String
prettyPrint term = case term of
    VarToT x -> show x
    Abstr arg body -> "\\" ++ getStringFromVar arg ++ " -> " ++ prettyPrint body
    App term1 term2 -> case term1 of
        VarToT _ -> case term2 of
            VarToT _ -> prettyPrint term1 ++ " " ++ prettyPrint term2
            Abstr _ _ -> prettyPrint term1 ++ " " ++ prettyPrint term2
            _ -> prettyPrint term1 ++ " (" ++ prettyPrint term2 ++ ")"
        Abstr _ _ -> "(" ++ prettyPrint term1 ++ ") " ++ prettyPrint term2
        App _ (Abstr _ _) -> case term2 of
            VarToT _ -> " (" ++ prettyPrint term1 ++ ") " ++ prettyPrint term2
            Abstr _ _ -> " (" ++ prettyPrint term1 ++ ") " ++ prettyPrint term2
            _ -> " (" ++ prettyPrint term1 ++ ") (" ++ prettyPrint term2 ++ ")"
        _ -> case term2 of
            VarToT _ -> prettyPrint term1 ++ " " ++ prettyPrint term2
            Abstr _ _ -> prettyPrint term1 ++ " " ++ prettyPrint term2
            _ -> prettyPrint term1 ++ " (" ++ prettyPrint term2 ++ ")"
