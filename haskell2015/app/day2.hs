{-# OPTIONS_GHC -Wno-name-shadowing -Wno-incomplete-patterns #-}
module Day2 where

-- 2015, Day 2
-- Input is a list of box dimensions. We need to calculate volumes, perimeters and
-- surfaces in order to wrap the boxes with gift paper and a nice bow tie.
--
-- Part1: Calculate amount of wrapping paper needed for all boxes. It's all surfaces + smallest surface.
-- Part2: Calculate amount of ribbon needed. It's smallest perimeter + volume.

import Shared (splitString, pairs)

type Dimensions = [Int]

parseLine :: String -> Dimensions
parseLine line = do
  -- Splits line on the 'x' char.
  let dim = splitString (=='x') line
  map read dim

double :: Int -> Int
double x = x + x

-- surfaces returns the areas of the 6 surfaces of the prism
surfaces :: Dimensions -> [Int]
surfaces dims = sf ++ sf
  where
    sf = map product (pairs dims)

-- perimeters returns the perimeters along the 3 dimensions of the prism
perimeters :: [Int] -> [Int]
perimeters dims = map (\x -> double (sum x)) (pairs dims)

part1 :: [Dimensions] -> Int
part1 inputs = sum (map paperNeeded inputs)
  where
    paperNeeded :: Dimensions -> Int
    paperNeeded dims = minimum (surfaces dims) + (sum (surfaces dims))

part2 :: [Dimensions] -> Int
part2 inputs = sum (map ribbonNeeded inputs)
  where
    ribbonNeeded :: Dimensions -> Int
    ribbonNeeded dims = minimum (perimeters dims) + product dims

day2 :: String -> (String, Int, Int)
day2 input = do
  let dims = map parseLine (lines input)
  ("Day 2: I Was Told There Would Be No Math", part1 dims, part2 dims)
