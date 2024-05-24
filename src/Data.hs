{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Data where


newtype V = Var { getStringFromVar :: String } deriving(Eq)

instance Show V where
    show :: V -> String
    show = getStringFromVar

data T = VarToT V | Abstr V T | App T T deriving (Eq, Show)
