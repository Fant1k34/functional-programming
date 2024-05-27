{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use guards" #-}
module Lib where

import Data
import Data.Char

import Control.Monad.Trans.State.Lazy (State, get, put)
-- prettyPrint:
-- переменная: x (или y) -> без скобок
-- Абстракция: \x . T -> без скобок
-- Аппликация: T T -> возможны скобки, рассмотрим ниже

    -- T           T
    -- x           y       -> без скобок
    -- x           \x . T  -> без скобок
    -- x           (T T)   -> со скобками справа
    -- (\x . T)    y       -> со скобками слева
    -- (\x . T)    (T T)   -> со скобками

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
        Abstr _ _ -> case term2 of
            App _ _ -> "(" ++ prettyPrint term1 ++ ") (" ++ prettyPrint term2 ++ ")"
            _ -> "(" ++ prettyPrint term1 ++ ") " ++ prettyPrint term2
        App _ (Abstr _ _) -> case term2 of
            VarToT _ -> " (" ++ prettyPrint term1 ++ ") " ++ prettyPrint term2
            Abstr _ _ -> " (" ++ prettyPrint term1 ++ ") " ++ prettyPrint term2
            _ -> " (" ++ prettyPrint term1 ++ ") (" ++ prettyPrint term2 ++ ")"
        _ -> case term2 of
            VarToT _ -> prettyPrint term1 ++ " " ++ prettyPrint term2
            Abstr _ _ -> prettyPrint term1 ++ " " ++ prettyPrint term2
            _ -> prettyPrint term1 ++ " (" ++ prettyPrint term2 ++ ")"


eagerReduction :: T -> State Int T
eagerReduction (VarToT x) = do
    return $ VarToT x

eagerReduction (Abstr arg body) = do
    reducted <- eagerReduction body

    return $ Abstr arg reducted

eagerReduction (App (VarToT x) term2) = do
    reducted <- eagerReduction term2

    return $ App (VarToT x) reducted


eagerReduction (App (Abstr arg body) term2) = do
    reductedTerm <- eagerReduction term2
    substituted <- substitute arg reductedTerm body

    eagerReduction $ substituted

eagerReduction (App term1 term2) = do
    reductedTerm1 <- eagerReduction term1
    reductedTerm2 <- eagerReduction term2

    case reductedTerm1 of
        Abstr arg body -> do
            let reductedTerm = reductedTerm2
            substituted <- substitute arg reductedTerm body

            eagerReduction $ substituted
        _ -> return $ App reductedTerm1 reductedTerm2


lazyReduction :: T -> State Int T
lazyReduction (VarToT x) = return $ VarToT x
lazyReduction (Abstr arg body) = do
    reducted <- lazyReduction body

    return $ Abstr arg reducted

lazyReduction (App (VarToT x) term2) = do
    reducted <- lazyReduction term2

    return $ App (VarToT x) reducted

lazyReduction (App (Abstr arg body) term2) = do
    substituted <- substitute arg term2 body

    lazyReduction substituted

lazyReduction (App term1 term2) = do
    reductedTerm1 <- lazyReduction term1

    case reductedTerm1 of
        Abstr arg body -> do
            substituted <- substitute arg term2 body

            lazyReduction substituted
        _ -> do
            reducted <- lazyReduction term2

            return $ App reductedTerm1 reducted


isFreeVar :: V -> T -> Bool
isFreeVar a (VarToT x) = a == x
isFreeVar a (App t1 t2) = isFreeVar a t1 || isFreeVar a t2
isFreeVar a (Abstr arg body) = if a == arg then False else isFreeVar a body


isBoundVar :: V -> T -> Bool
isBoundVar a (VarToT x) = False
isBoundVar a (App t1 t2) = isBoundVar a t1 || isBoundVar a t2
isBoundVar a (Abstr arg body) = if a == arg then True else isBoundVar a body


isReductable :: T -> Bool
isReductable (VarToT x) = False
isReductable (Abstr arg body) = isReductable body
isReductable (App (Abstr _ _) _) = True
isReductable (App term1 term2) = isReductable term1 || isReductable term2



getSubstitutionVar :: T -> T -> State Int V
getSubstitutionVar term1 term2 = do
    ind <- get

    let var = Var ((filter (\el -> let elVar = Var $ "x" ++ el
                                                  in
                                                  (not (isFreeVar elVar term1)) && (not (isBoundVar elVar term1))
                                                  && (not (isFreeVar elVar term2)) && (not (isBoundVar elVar term2))) [[x1] ++ [x2] ++ [x3] ++ [x4] ++ [x5] | x1 <- ['a'..'z'], x2 <- ['a'..'z'], x3 <- ['a'..'z'], x4 <- ['a'..'z'], x5 <- ['a'..'z']]) !! ind)

    put (ind + 1)

    return var
    


substitute :: V -> T -> T -> State Int T
--  [x -> N] x = N
--  [x -> N] y = y
substitute arg body (VarToT var) = if arg == var then return $ body else return $ VarToT var

--  [x -> N] (PQ) = ([x -> N] P) ([x -> N] Q)
substitute arg body (App term1 term2) = do
    substituted1 <- substitute arg body term1
    substituted2 <- substitute arg body term2

    return $ App substituted1 substituted2

--  [x -> N] (λx P) = λx . P
-- [x -> N] (λy . P) = λy . [x -> N] P, if y ∉ FV(N)
-- [x -> N] (λy . P) = λz . [x -> N] ([y -> z] P), if y ∈ FV(N), z ∉ FV(N) ∪ FV(P)
substitute x body (Abstr y body') = do
    if x == y then return $ Abstr y body' else
        if not $ isFreeVar y body then (do
            substituted <- substitute x body body'
            
            return $ Abstr y substituted
            )
            else (do
                new <- getSubstitutionVar body body'
                substitutedInner <- substitute y (VarToT new) body'
                substitutedOuter <- substitute x body substitutedInner
                
                return $ Abstr new substitutedOuter
                )
