module Main where

import Day1 (day1)
import Day2 (day2)
import Day3 (day3)
import Day4 (day4)
import Day5 (day5)
import Day6 (day6)
import Day7 (day7)
import Day8 (day8)
import Day9 (day9)
import Day10 (day10)
import Day11 (day11)
import Day12 (day12)

-- day applies a day function to an input file.
day :: Show a => String -> (String -> (String, a, a)) -> IO ()
day filename dayFn = do
    contents <- readFile filename
    let (title, part1, part2) = dayFn contents
    putStr $ title ++ "\n  Part 1: " ++ (show part1) ++ "\n  Part 2: " ++ (show part2) ++ "\n"

main :: IO()
main = do
  day "input/day1.txt" day1
  day "input/day2.txt" day2
  day "input/day3.txt" day3
  day "input/day4.txt" day4
  day "input/day5.txt" day5
  day "input/day6.txt" day6
  day "input/day7.txt" day7
  day "input/day8.txt" day8
  day "input/day9.txt" day9
  day "input/day10.txt" day10
  day "input/day11.txt" day11
  day "input/day12.txt" day12
  return ()
