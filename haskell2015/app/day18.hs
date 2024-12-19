module Day18 where

import qualified Data.Set as S
import Data.Set (Set)

type Coord = (Int, Int)
type Grid = ((Int, Int), Set Coord)

readGrid :: String -> Grid
readGrid input = do
  ((width, height), cells')
    where
      cells = lines input
      isAlive :: Coord -> Bool
      isAlive (x, y) = (cells !! y) !! x == '#'
      width = length (cells !! 0)
      height = length cells
      cells' = S.fromList $ filter isAlive $ [(x, y) | x <- [0..width - 1], y <- [0..height - 1]]

step :: Grid -> Grid
step ((width, height), cells) = ((width, height), cells')
  where
    cells' = S.fromList $ filter (\c -> (staysOn c) || (turnsOn c)) $ [(x, y) | x <- [0..width -1], y <- [0..height - 1]]
    staysOn :: Coord -> Bool
    staysOn c = (isOn c) && (nOnNeighbors == 2 || nOnNeighbors == 3)
      where
        nOnNeighbors = length $ filter isOn $ neighbors c
    turnsOn :: Coord -> Bool
    turnsOn c = (not $ isOn c) && (length $ filter isOn $ neighbors c) == 3
    isOn :: Coord -> Bool
    isOn c = S.member c cells
    -- neighbors returns all neighboring coords of c
    neighbors :: Coord -> [Coord]
    neighbors c = map (add c) [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]
      where
        add :: Coord -> Coord -> Coord
        add (ax, ay) (bx, by) = (ax+bx, ay+by)

lightCount :: Grid -> Int
lightCount (_, cells) = S.size cells

part1 :: Grid -> Int
part1 grid = lightCount $ (iterate step grid) !! 100

part2 :: Grid -> Int
part2 grid = lightCount $ fixCorners $ (iterate (step . fixCorners) grid) !! 100
  where
    fixCorners :: Grid -> Grid
    fixCorners (dim, cells) = (dim, S.union corners cells)
    corners = S.fromList [(0, 0), (0, height-1), (width-1, 0), (width-1, height-1)]
    ((width, height), _)  = grid

day18 :: String -> (String, Int, Int)
day18 input = do
  let grid = readGrid input
  ("Day 18: Like a GIF For Your Yard", part1 grid, part2 grid)
