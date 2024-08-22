module Day3 where

-- 2015, Day 3
-- Santa and RoboSanta are moving from house to house. They follow a path over a manhattan grid.
--
-- Part 1: How many houses are visited if Santa follows every instruction.
-- Part 2: Which houses are visited if Santa and RoboSanta take turns following instructions.

import Data.Set (Set, singleton, size, insert)

type Coord = (Int, Int)
type CoordSet = Set Coord

update :: Coord -> Char -> Coord
update (x, y) '^' = (x, y - 1)
update (x, y) 'v' = (x, y + 1)
update (x, y) '<' = (x - 1, y)
update (x, y) '>' = (x + 1, y)

startingCoord :: Coord
startingCoord = (0, 0)

startingHouse :: CoordSet
startingHouse = singleton startingCoord

part1 :: String -> Int
part1 moves = inner moves startingCoord startingHouse
  where
    inner :: String -> Coord -> CoordSet -> Int
    inner "" _ visitedHouses = size visitedHouses
    inner (move : moves) santa visitedHouses = inner moves santa' (insert santa' visitedHouses)
      where santa' = update santa move

part2 :: String -> Int
part2 x = inner x startingCoord startingCoord startingHouse
  where
    inner :: String -> Coord -> Coord -> CoordSet -> Int
    inner "" _ _ visitedHouses = size visitedHouses
    -- Santa and Robo-santa alternate moves.
    inner (move : moves) santa1 santa2 set = inner moves santa2 santa1' (insert santa1' set)
      where santa1' = update santa1 move

day3 :: String -> (String, Int, Int)
day3 input = do
  let line = (lines input) !! 0
  ("Day 3: Perfectly Spherical Houses in a Vacuum", part1 line, part2 line)
