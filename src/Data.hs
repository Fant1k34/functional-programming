{-# LANGUAGE InstanceSigs #-}
module Data where


data T = Var String | Abstr [String] T | App [T] deriving (Eq, Show)
