{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

-- Exercise 2
countIf :: (a -> Bool) -> [a] -> Int
countIf p = length . filter p

count :: Eq a => a -> [a] -> Int
count x = countIf (==x)

-- The attacking player may attack with up to three units at a time.
-- However, they must always leave at least one unit behind
availAttack :: Battlefield -> Int
availAttack = min 3 . (subtract 1) . attackers

-- The defending player may defend with up to two units
-- (or only one if that is all they have).
availDefend :: Battlefield -> Int
availDefend = min 2 . defenders

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)

data Side = A | D
  deriving Eq

-- Determines the winner for a pair of rolls (the defending player wins ties).
fight :: DieValue -> DieValue -> Side
fight a d
  | a > d     = A
  | otherwise = D

battle :: Battlefield -> Rand StdGen Battlefield
battle b =
  replicateM (availAttack b) die >>= \rollsA ->
  replicateM (availDefend b) die >>= \rollsD ->
  let outcomes = zipWith fight (sortDesc rollsA) (sortDesc rollsD)
      winsA    = count A outcomes
      winsD    = count D outcomes
  in return $ Battlefield (attackers b - winsD) (defenders b - winsA)

-- Exercise 3
invade :: Battlefield -> Rand StdGen Battlefield
invade = iterateUntil complete battle
  where
    complete bf = attackers bf < 2 || defenders bf == 0

iterateUntil :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntil p f b
  | p b       = return b
  | otherwise = f b >>= iterateUntil p f

-- Exercise 4
destroyed :: Battlefield -> Bool
destroyed b = defenders b == 0

-- Calculates the fraction of elements of `xs` that satisfy `p`.
frac :: (a -> Bool) -> [a] -> Double
frac p xs =
  let n = fromIntegral $ countIf p xs
      d = fromIntegral $ length xs
  in n / d

successProb :: Battlefield -> Rand StdGen Double
successProb b =
  replicateM 1000 (invade b) >>= \results ->
  return $ frac destroyed results

-- main :: IO ()
-- main =
--   evalRandIO (successProb (Battlefield 10 10)) >>= \prob ->
--   putStrLn $ show prob
