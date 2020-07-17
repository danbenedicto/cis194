{-# OPTIONS_GHC -Wall #-}

module Homework4 where

import Data.List ((\\))

fun1 :: [Integer] -> Integer
-- fun1 []     = 1
-- fun1 (x:xs)
--   | even x    = (x - 2) * fun1 xs
--   | otherwise = fun1 xs
fun1 = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
-- fun2 1 = 0
-- fun2 n | even n    = n + fun2 (n `div` 2)
--        | otherwise = fun2 (3 * n + 1)
fun2 =
  sum .
  filter even .
  takeWhile (/= 1) .
  iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

createNode :: a -> Tree a
createNode a = Node 0 Leaf a Leaf

insert :: a -> Tree a -> Tree a
insert a tree = case tree of
  Leaf -> createNode a
  (Node _ Leaf x Leaf)  -> (Node 1 (createNode a) x Leaf)
  (Node h Leaf x right) -> (Node h (createNode a) x right)
  (Node h left x Leaf)  -> (Node h left x (createNode a))
  (Node _ left@(Node hL _ _ _) x right@(Node hR _ _ _))
    | hL <= hR  -> (Node (hLNew + 1) newLeft x right)
    | otherwise -> (Node (hRNew + 1) left x newRight)
    where newLeft@(Node hLNew _ _ _)  = (insert a left)
          newRight@(Node hRNew _ _ _) = (insert a right)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

-- Provided for `sieveSundaram`
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) $ [1..n] \\ elim
  where elim = filter (<=n)
             . map (\(i, j) -> i + j + 2*i*j)
             $ cartProd [1..n] [1..n]
