module Main where

import Day1 (day1)
import Day2 (day2)
import Day3 (day3)
import Shared (runDay)

main :: IO()
main = do
  runDay 1 "input/day1.txt" day1
  runDay 2 "input/day2.txt" day2
  runDay 3 "input/day3.txt" day3
  return ()
