module Day9 where

-- 2015, Day 9
-- Each line of the input specifies the distance between two cities. We walk through each possible permutation of
--   cities to determine the shortest and longest distances.
--
-- Part 1: Length of the shortest path.
-- Part 2: Length of the longest path.

import qualified Data.Map as M
import qualified Data.Set as S
import Shared (splitWords)
import Data.Map ((!))

type City = String
type Distances = M.Map City [(City, Int)]
type Visited = S.Set City

-- reads the lines and builds a map of each city to
parseInput :: String -> Distances
parseInput input = foldl storePair M.empty (lines input)
  where
    storePair :: Distances -> String -> Distances
    storePair dists line = storeDistance (storeDistance dists city1 city2 dist) city2 city1 dist
      where
        [city1, _, city2, _, d] = splitWords (==' ') line
        dist = read d
        storeDistance :: Distances -> City -> City -> Int -> Distances
        storeDistance dists city1 city2 dist =
          case (M.lookup city1 dists) of
            Just list -> M.insert city1 (list ++ [(city2, dist)]) dists
            Nothing -> M.insert city1 [(city2, dist)] dists

-- bestDistance walks through the graph and returns either the shortest or longest path.
bestDistance :: Distances -> (Int -> Int -> Int) -> Int -> Int
bestDistance dists fn worst = foldl fn worst (map (\startCity -> search (S.fromList[startCity]) startCity 0) (M.keys dists))
  where
    search :: Visited -> City -> Int -> Int
    search visited curCity curDist
      -- we've visited all cities, return curDist
      | length visited == length dists = curDist
      | otherwise = foldl fn worst (map (\(nxtCity, d) -> search (S.insert nxtCity visited) nxtCity (curDist+d)) unvisited)
        where
          unvisited = filter (\(city, _) -> S.notMember city visited) (dists ! curCity)

part1 :: Distances -> Int
part1 dists = bestDistance dists min maxBound

part2 :: Distances -> Int
part2 dists = bestDistance dists max 0

day9 :: String -> (String, Int, Int)
day9 input = do
  let distances = parseInput input
  ("Day 9: All in a single night", part1 distances, part2 distances)
