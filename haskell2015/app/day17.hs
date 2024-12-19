module Day17 where

-- 2015, Day 17. The well-known count-of-coins puzzle, but this time expressed
--  as buckets that contain eggnog.

-- Part 1: Number of possible ways taking all buckets into account.
-- Part 2: Number of possible ways using the least possible number of buckets.

import Data.List (sort)

readBuckets :: String -> [Int]
readBuckets input = sort $ map read $ lines input

-- countCombinations counts the number of ways in which the buckets can be fully filled to
--  contain exactly the amount of eggnog.
-- The maxNBuckets parameter indicates the max. number of buckets that may be used.
countCombinations :: [Int] -> Int -> Int -> Int
countCombinations buckets eggnog maxNBuckets
  | eggnog < 0          = 0
  | maxNBuckets < 0     = 0
  | eggnog == 0         = 1
  | length buckets == 0 = 0
  | otherwise           = (countCombinations (drop 1 buckets) eggnog maxNBuckets)
                          + (countCombinations (drop 1 buckets) (eggnog - (buckets !! 0)) (maxNBuckets - 1))

part1 :: [Int] -> Int
part1 buckets = countCombinations buckets 150 (length buckets)

part2 :: [Int] -> Int
part2 buckets = head $ filter (>0) $ map (countCombinations buckets 150) [1..]

day17 :: String -> (String, Int, Int)
day17 input = do
  let buckets = readBuckets input
  ("Day 17: No Such Thing as Too Much", part1 buckets, part2 buckets)
