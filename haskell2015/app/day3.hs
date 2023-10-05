module Day3 where

-- 2015, Day 3
-- Santa and RoboSanta are moving from house to house. They follow a path over a manhattan grid.
--
-- Part 1: Which houses are visited if Santa follows every instruction.
-- Part 2: Which houses are visited if Santa and RoboSanta take turns following instructions.

import Data.Set (Set, singleton, size, insert)

type Coord = (Int, Int)
type CoordSet = Set Coord

update :: Coord -> Char -> Coord
update (x, y) '^' = (x, y - 1)
update (x, y) 'v' = (x, y + 1)
update (x, y) '<' = (x - 1, y)
update (x, y) '>' = (x + 1, y)

part1 :: String -> Int
part1 x = inner x (0, 0) (singleton (0, 0))
  where
    inner :: String -> Coord -> CoordSet -> Int
    inner "" _ set = size set
    inner (move : moves) src set = inner moves dst (insert dst set)
      where dst = update src move

part2 :: String -> Int
part2 x = inner x (0, 0) (0, 0) (singleton (0, 0))
  where
    inner :: String -> Coord -> Coord -> CoordSet -> Int
    inner "" _ _ set = size set
    inner (move : moves) src1 src2 set = inner moves src2 dst (insert dst set)
      where dst = update src1 move

day3 :: String -> (Int, Int)
day3 input = do
  let line = (lines input) !! 0
  (part1 line, part2 line)
