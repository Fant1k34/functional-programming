{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use guards" #-}
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


eagerReduction :: T -> T
eagerReduction (VarToT x) = VarToT x
eagerReduction (Abstr arg body) = Abstr arg body

eagerReduction (App (VarToT x) term2) = App (VarToT x) term2
eagerReduction (App (Abstr arg body) term2) = do
    let reductedTerm = term2

    substitute arg reductedTerm body

eagerReduction (App term1 term2) = do
    let reductedTerm1 = eagerReduction term1
    let reductedTerm2 = eagerReduction term2

    case reductedTerm1 of
        Abstr arg body -> do
            let reductedTerm = term2

            substitute arg reductedTerm body
        _ -> App reductedTerm1 reductedTerm2



lazyReduction :: T -> T
lazyReduction (VarToT x) = VarToT x
lazyReduction (Abstr arg body) = Abstr arg body

lazyReduction (App (VarToT x) term2) = App (VarToT x) term2
lazyReduction (App (Abstr arg body) term2) = do
    let evaled = substitute arg term2 body

    lazyReduction evaled

lazyReduction (App term1 term2) = do
    let reductedTerm1 = lazyReduction term1

    case reductedTerm1 of
        Abstr arg body -> do
            let evaled = substitute arg term2 body

            lazyReduction evaled
        _ -> App reductedTerm1 (lazyReduction term2)


isFreeVar :: V -> T -> Bool
isFreeVar a (VarToT x) = a == x
isFreeVar a (App t1 t2) = isFreeVar a t1 || isFreeVar a t2
isFreeVar a (Abstr arg body) = if a == arg then False else isFreeVar a body


substitute :: V -> T -> T -> T
--  [x -> N] x = N
--  [x -> N] y = y
substitute arg body (VarToT var) = if arg == var then body else VarToT var

--  [x -> N] (PQ) = ([x -> N] P) ([x -> N] Q)
substitute arg body (App term1 term2) = App (substitute arg body term1) (substitute arg body term2)

--  [x -> N] (λx P) = λx . P
-- [x -> N] (λy . P) = λy . [x -> N] P, if y ∉ FV(N)
-- [x -> N] (λy . P) = λz . [x -> N] ([y -> z] P), if y ∈ FV(N), z ∉ FV(N) ∪ FV(P)

substitute x body (Abstr y body') = if x == y then Abstr y body' else
    if not $ isFreeVar y body then substitute x body body' 
        else let new = Var "new" 
             in
             Abstr new (substitute x body (substitute y (VarToT new) body'))


