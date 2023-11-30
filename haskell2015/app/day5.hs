module Day5 where

--  2015, Day 5
-- Input is a list of random strings. These can be nice or not, the goal is to count the number of nice strings
--
-- Part 1: A nice string contains at least 3 vowels, a letter that appears twice consecutively, but none of the substrings "ab", "cd", "pq", "xy".
-- Part 2: A nice string contains a recurring pair of letters that is not overlapping, and the same letter appears twice with another letter between them.

import qualified Data.Map as Map

count :: (a -> Bool) -> [a] -> Int
count fn l = length (filter fn l)

matchesAny :: Eq a => [a] -> (a -> Bool)
matchesAny col = (\ e -> any (\ v -> v == e) col)

consecutivePairs :: [a] -> [(a, a)]
consecutivePairs list = zip list (drop 1 list)

consecutiveTriples :: [a] -> [(a, a, a)]
consecutiveTriples list = zip3 list (tail list) (tail (tail list))

part1 :: [String] -> Int
part1 strings = length (filter isNice strings) where
  isNice :: String -> Bool
  isNice string = countVowels >= 3 &&
                  hasDoubleLetter &&
                  not doesContain
    where
      countVowels = count (matchesAny "aeiou") string
      hasDoubleLetter = any (\ (a, b) -> a == b) (consecutivePairs string)
      doesContain = any (matchesAny (map tuplify ["ab", "cd", "pq", "xy"])) (consecutivePairs string) where
        tuplify :: [a] -> (a, a)
        tuplify [x, y] = (x, y)

hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates list = inner list 0 Map.empty where
  inner :: Ord a => [a] -> Int -> (Map.Map a Int)  -> Bool
  inner [] _ _ = False
  inner (x : xs) idx m = case Map.lookup x m of
                           Nothing -> inner xs (idx + 1) (Map.insert x idx m)
                           Just jdx
                             | jdx == idx - 1 -> inner xs (idx + 1) m
                             | otherwise -> True

part2 :: [String] -> Int
part2 strings = length (filter isNice strings) where
  isNice :: String -> Bool
  isNice string = hasDuplicates (consecutivePairs string) &&
                  any (\ (a, _, c) -> a == c) (consecutiveTriples string)

day5 :: String -> (Int, Int)
day5 input = do
  let strings = (lines input)
  (part1 strings, part2 strings)
