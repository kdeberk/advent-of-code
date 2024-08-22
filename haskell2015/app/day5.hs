module Day5 where

-- 2015, Day 5
-- Input is a list of random strings. These can be nice or not, the goal is to count the number of nice strings
--
-- Part 1: A nice string contains at least 3 vowels, a letter that appears twice consecutively, but none of the substrings "ab", "cd", "pq", "xy".
-- Part 2: A nice string contains a recurring pair of letters that is not overlapping, and the same letter appears twice with another letter between them.

import qualified Data.Set as Set

-- count counts how many times the predicate evaluated to true.
count :: (a -> Bool) -> [a] -> Int
count pred l = length (filter pred l)

-- matchesAny creates a predicate that returns true if the input is in the given set.
matchesAny :: Eq a => [a] -> (a -> Bool)
matchesAny col = (\e -> any (\v -> v == e) col)

-- consecutivePairs returns a pair for each two consecutive items in the list
consecutivePairs :: [a] -> [(a, a)]
consecutivePairs list = zip list (tail list)

-- consecutivePairs returns... you guessed it, a pair for each three consecutive items in the list
consecutiveTriples :: [a] -> [(a, a, a)]
consecutiveTriples list = zip3 list (tail list) (tail (tail list))

part1 :: [String] -> Int
part1 strings = length (filter isNice strings) where
  isNice :: String -> Bool
  isNice string = vowelCount >= 3 &&
                  hasDoubleLetters &&
                  not doesContain
    where
      vowelCount = count (matchesAny "aeiou") string
      hasDoubleLetters = any (\(a, b) -> a == b) (consecutivePairs string)
      doesContain = any (matchesAny (map tuplify ["ab", "cd", "pq", "xy"])) (consecutivePairs string) where
        tuplify :: [a] -> (a, a)
        tuplify [x, y] = (x, y)

part2 :: [String] -> Int
part2 strings = length (filter isNice strings) where
  isNice :: String -> Bool
  isNice string = (hasDuplicates (nonoverlappingPairs string)
                    || hasDuplicates (nonoverlappingPairs (tail string)))
                  && (any (\ (a, _, c) -> a == c) (consecutiveTriples string))
     where
       nonoverlappingPairs :: [a] -> [(a, a)]
       nonoverlappingPairs list = zip list (drop 2 list)
       -- hasDuplicates returns true if the list contains the same value twice or more.
       hasDuplicates :: Ord a => [a] -> Bool
       hasDuplicates list = not (setSize == listSize)
         where
           setSize = Set.size (Set.fromList list)
           listSize = length list

day5 :: String -> (String, Int, Int)
day5 input = do
  let strings = (lines input)
  ("Day 5: Doesn't He Have Intern-Elves For This?", part1 strings, part2 strings)
