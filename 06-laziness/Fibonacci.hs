{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-methods #-}

module Fibonacci where

import Data.List (intercalate)

-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2

fibs2 :: [Integer]
fibs2 = map fst $ iterate (\(a, b) -> (b, a+b)) (0,1)
-- A really cool implementation I found online:
-- fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a bs) = a : (streamToList bs)

instance Show a => Show (Stream a) where
  show = (\s -> "stream # [" ++ s ++ ",...]")
       . intercalate ","
       . map show
       . take 20
       . streamToList

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a bs) = Cons (f a) $ streamMap f bs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed unfold seed = Cons seed $ streamFromSeed unfold (unfold seed)

-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- Ruler function 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, ...
-- where the nth element in the stream (assuming the first element
-- corresponds to n=1) is the largest power of 2 which evenly
-- divides n.
ruler :: Stream Integer
ruler = foldr interleaveStreams undefined $ map streamRepeat [0..]

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a0 a') b =
  Cons a0 $ interleaveStreams b a'

-- Exercise 6 (Optional)

x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger n = Cons n $ streamRepeat 0
  negate = streamMap negate
  (+) (Cons a0 a')   (Cons b0 b') = Cons (a0 + b0) $ a' + b'
  (*) (Cons a0 a') b@(Cons b0 b') = Cons (a0 * b0) $ (streamMap (a0*) b') + (a' * b)

instance Fractional (Stream Integer) where
  (/) (Cons a0 a') (Cons b0 b') = q
    where q = Cons (a0 `div` b0) $ streamMap (`div` b0) (a' - q*b')

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x*x)

-- Exercise 7 (Extra credit)

data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
  (*) (Matrix a0 a1 a2 a3) (Matrix b0 b1 b2 b3) =
    Matrix (a0*b0 + a1*b2) (a2*b0 + a3*b2) (a0*b1 + a1*b3) (a2*b1 + a3*b3)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = getFn $ (Matrix 1 1 1 0)^n
  where getFn (Matrix _ a _ _) = a
