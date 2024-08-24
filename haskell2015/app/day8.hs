module Day8 where

-- 2015, Day 8
-- Input consists of strings containing normal ascii plus  \", \\ and \xNN encoded characters.
--
-- Part 1: Calculate size difference between literal and decoded strings.
--   We're not actually decoding the string, merely keep track of size impact as we scan through the string.
-- Part 2: Encode the encoded literal strings and return the difference in size with the original literals.

import Data.List (isPrefixOf)

countDecodedChars :: String -> Int
countDecodedChars str = rollingCount str (-2) -- start with -2 to offset the wrapping quotes
  where
    rollingCount :: String -> Int -> Int
    rollingCount str count
      | str == "" = count
        -- TODO: need to determine that the 2 following chars are digits, but that's somehow not necessary for the challenge.
      | isPrefixOf "\\x" str = rollingCount (drop 4 str) (count + 1)
      | isPrefixOf "\\"  str = rollingCount (drop 2 str) (count + 1) -- handles both \\ and \"
      | otherwise = rollingCount (drop 1 str) (count + 1)

countEncodedChars :: String -> Int
countEncodedChars str = rollingCount str 2 -- start with 2 to include the new wrapping quotes
  where
    rollingCount :: String -> Int -> Int
    rollingCount str count
      | str == "" = count
      | isPrefixOf "\"" str = rollingCount (drop 1 str) (count + 2)  -- " -> \"
      | isPrefixOf "\\" str = rollingCount (drop 1 str) (count + 2)  -- \ -> \\
      | otherwise = rollingCount (drop 1 str) (count + 1)

part1 :: [String] -> Int
part1 lines = sum (map (\line -> (length line) - (countDecodedChars line)) lines)

part2 :: [String] -> Int
part2 lines = sum (map (\line -> (countEncodedChars line) - (length line)) lines)

day8 :: String -> (String, Int, Int)
day8 input = do
  let strings = lines input
  ("Day 8: Matchsticks", part1 strings, part2 strings)
