{-# OPTIONS_GHC -Wno-name-shadowing -Wno-incomplete-patterns #-}
module Day1 where

-- 2015, Day 1
-- Input is a list of parentheses. Keep track of number of open vs close parentheses.
--
-- Part 1: Count number of opens and subtract number of closes.
-- Part 2: Find first index where there are more close parentheses than open.

part1 :: String -> Int
part1 x = inner x 0
      where
        inner :: String -> Int -> Int
        inner "" floor = floor
        inner ('(' : x) floor = inner x (floor + 1)
        inner (')' : x) floor = inner x (floor - 1)

part2 :: String -> Int
part2 x = inner x 0 0
      where
        inner :: String -> Int -> Int -> Int
        -- We've reached floor -1, return idx.
        inner _ (-1) idx = idx
        inner ('(' : x) floor idx = inner x (floor + 1) (idx + 1)
        inner (')' : x) floor idx = inner x (floor - 1) (idx + 1)

day1 :: String -> (Int, Int)
day1 input = do
     let line = (lines input) !! 0
     (part1 line, part2 line)
