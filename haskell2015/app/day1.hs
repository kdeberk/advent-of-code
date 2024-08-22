{-# OPTIONS_GHC -Wno-name-shadowing -Wno-incomplete-patterns #-}
module Day1 where

-- 2015, Day 1
-- Input is a list of parentheses. ( means go floor up, and ) go floor down.
--   Keep track of number of open vs close parentheses.
--
-- Part 1: The floor on which Santa ends: curFloor number of opens and subtract number of closes.
-- Part 2: Find index that takes Santa to the basement: where there are more close parentheses than open.

part1 :: String -> Int
part1 x = santa x 0
      where
        -- Santa keeps track of open vs. close parens.
        santa :: String -> Int -> Int
        santa "" curFloor         = curFloor
        santa ('(' : xs) curFloor = santa xs (curFloor + 1)
        santa (')' : xs) curFloor = santa xs (curFloor - 1)

part2 :: String -> Int
part2 x = santa x 0 0
      where
        santa :: String -> Int -> Int -> Int
        -- We've reached floor -1, return idx.
        santa _ (-1) idx = idx
        santa ('(' : x) curFloor idx = santa x (curFloor + 1) (idx + 1)
        santa (')' : x) curFloor idx = santa x (curFloor - 1) (idx + 1)

day1 :: String -> (String, Int, Int)
day1 input = do
     let line = (lines input) !! 0
     ("Day 1: Not Quite Lisp", part1 line, part2 line)
