{-# OPTIONS_GHC -Wall #-}

module Homework1 where

toDigits :: Integer -> [Integer]
toDigits n
    | n >= 10   = (toDigits d) ++ [r]
    | n > 0     = [n]
    | otherwise = []
  where (d, r) = divMod n 10

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

double :: Integer -> Integer
double n = n + n

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther l
    | length l `mod` 2 == 0 = (double x) : (doubleEveryOther ys)
    | otherwise             = x : (doubleEveryOther ys)
  where (x:ys) = l

sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:ys) = sum (toDigits x) + sumDigits(ys)

validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (toDigits n))) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a, b)] ++ (hanoi (n-1) c b a)
