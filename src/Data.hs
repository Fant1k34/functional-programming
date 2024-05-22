{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Data where

data (Integral a) => IntegerWithBase a = Number a a


countNumberFromBase :: (Integral a) => a -> [a] -> [a]
countNumberFromBase leftoverOfNumber list
  | leftoverOfNumber < 0 = []
  | leftoverOfNumber < 10 = leftoverOfNumber : list
  | otherwise = countNumberFromBase (div leftoverOfNumber 10) (mod leftoverOfNumber 10 : list)


razryad :: Integral a => a -> a
razryad value = if value < 10 then 1 else razryad (div value 10) + 1


evaluate :: Integral a => a -> a -> a -> a
evaluate base value stepen = value * base ^ stepen


toDecimal :: (Integral a) => IntegerWithBase a -> a
toDecimal (Number value base) = sum $ zipWith (evaluate base) (countNumberFromBase value []) (reverse [0..(razryad value - 1)])



fromDecimal' :: (Integral a) => a -> a -> [a] -> [a]
fromDecimal' value base list = if value == 0 then list else fromDecimal' (div value base) base (mod value base : list)


fromDecimal :: (Integral a) => a -> a -> IntegerWithBase a
fromDecimal value baseTo = Number (foldl1 (\prev curr -> prev * 10 + curr) (fromDecimal' value baseTo [])) baseTo


instance (Show a, Integral a) => Show (IntegerWithBase a) where
  show :: IntegerWithBase a -> String
  show = show . toDecimal


instance (Show a, Integral a) => Num (IntegerWithBase a) where
  (+) :: (Show a, Integral a) => IntegerWithBase a -> IntegerWithBase a -> IntegerWithBase a
  (+) (Number xValue xBase) (Number yValue yBase) = if xBase /= yBase then error "Unsupported operation" else fromDecimal (toDecimal (Number xValue xBase) + toDecimal (Number yValue yBase)) xBase

  (*) :: (Show a, Integral a) => IntegerWithBase a -> IntegerWithBase a -> IntegerWithBase a
  (*) (Number xValue xBase) (Number yValue yBase) = if xBase /= yBase then error "Unsupported operation" else fromDecimal (toDecimal (Number xValue xBase) * toDecimal (Number yValue yBase)) xBase
   
  negate :: (Show a, Integral a) => IntegerWithBase a -> IntegerWithBase a
  negate _ = error "Unsupported operation"

  abs :: (Show a, Integral a) => IntegerWithBase a -> IntegerWithBase a
  abs x = x

  signum :: (Show a, Integral a) => IntegerWithBase a -> IntegerWithBase a
  signum _ = error "Unsupported operation"

  fromInteger :: (Show a, Integral a) => Integer -> IntegerWithBase a
  fromInteger _ = error "Unsupported operation"
