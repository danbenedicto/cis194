{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wall #-}

module JoinList where

import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a Empty = a
(+++) Empty b = b
(+++) a b = Append m' a b
  where m' = (tag a) <> (tag b)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- Exercise 2

sizeJ :: (Sized b, Monoid b) => JoinList b a -> Int
sizeJ = getSize . size . tag

indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ a) = Just a
indexJ i (Append m l r)
  | i < 0 || i >= fullSize = Nothing
  | i < sizeLeft           = indexJ i l
  | otherwise              = indexJ (i - sizeLeft) r
    where fullSize = getSize $ size m
          sizeLeft = sizeJ l
indexJ _ _ = Nothing

-- jl :: JoinList Size String
-- jl = Append (Size 4)
--       (Append (Size 2)
--               (Single (Size 1) "apple")
--               (Single (Size 1) "banana"))
--       (Append (Size 2)
--               (Single (Size 1) "cashew")
--               (Single (Size 1) "doughnut"))

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
dropJ n j
  | n <= 0       = j
  | n >= sizeJ j = Empty
dropJ n (Append _ l r)
  | n <=  sizeL = (dropJ n l) +++ r
  | otherwise   = dropJ (n - sizeL) r
    where sizeL = sizeJ l
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ n j
  | n <= 0       = Empty
  | n >= sizeJ j = j
takeJ n (Append _ l r)
  | n <= sizeL = takeJ n l
  | otherwise  = l +++ takeJ (n - sizeL) r
    where sizeL = sizeJ l
takeJ _ j = j

-- Exercise 3 (also see Scrabble.hs)

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- Exercise 4

fromLine :: String -> (JoinList (Score, Size) String)
fromLine s = Single (scoreString s, Size 1) s

splitHalf :: [a] -> ([a], [a])
splitHalf l = splitAt (length l `div` 2) l

foldOut :: (a -> a -> a) -> a -> [a] -> a
foldOut _ b []     = b
foldOut f b (x:[]) = f x b
foldOut f b xs     = f (foldOut f b l) (foldOut f b r)
  where (l, r) = splitHalf xs

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList

  fromString = foldOut (+++) Empty
             . map fromLine
             . lines

  line = indexJ

  replaceLine n l jl = (takeJ n jl) +++
                       (fromLine l) +++
                       (dropJ (n + 1) jl)

  numLines = sizeJ

  value j = n
    where (Score n) = fst $ tag j

main :: IO ()
main = runEditor editor $ fromLine "Hey, how's it going?"
