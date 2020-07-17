{-# OPTIONS_GHC -Wall #-}
module Golf where

-- chunk 2 "ABCDEF" == ["ABCDEF", "CDEF", "EF"]
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = xs : chunk n (drop n xs)

-- takeEvery 2 "ABCDEF" == "A", "C", "E"
takeEvery :: Int -> [a] -> [a]
takeEvery n = map head . chunk n

-- skips "ABCD"       == ["ABCD", "BD", "C", "D"]
-- skips "hello!"     == ["hello!", "el!", "l!", "l", "o", "!"]
-- skips [1]          == [[1]]
-- skips [True,False] == [[True,False], [False]]
-- skips []           == []
skips :: [a] -> [[a]]
skips = skipsN 1

skipsN :: Int -> [a] -> [[a]]
skipsN _ [] = []
skipsN n xs = (takeEvery n xs) : (skipsN (n + 1) (tail xs))

triples :: [Integer] -> [(Integer, (Integer, Integer))]
triples l@(_:_:_:_) = zip (tail (init l)) (zip (init l) (tail (tail l)))
triples _ = []

-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []
localMaxima :: [Integer] -> [Integer]
localMaxima = map fst . filter (\(n,(l,r)) -> n > l && n > r) . triples

data Counter = Counter [Integer]
  deriving Show

count :: Integer -> Counter -> Counter
count n counter@(Counter l)
  | nInt < length l = (Counter (left ++ ((i + 1) : right)))
  | otherwise    = counter
    where (left, (i:right)) = splitAt nInt l
          nInt = (fromIntegral n::Int)

countAll :: [Integer] -> Counter
countAll = foldr count (Counter [0,0,0,0,0,0,0,0,0,0])

-- count :: [Integer] -> [Int]
-- count xs = map (\n -> length $ filter (== n) xs) [0..9]

toRow :: Counter -> Integer -> String
toRow (Counter l) minCount = map (\c -> if c >= minCount then '*' else ' ') l

histogram :: [Integer] -> String
histogram xs =
  unlines (
    (map (toRow counter) [maxCount,maxCount-1..1] ) ++
    ["==========", "0123456789"])
  where counter@(Counter l) = countAll xs
        maxCount = maximum l
