module Day14 where

-- 2015, Day 14.
-- Reindeers are moving in alternations of sprints and then rests.
-- Part 1: What distance did the fastest reindeer cover after 2503 seconds.
-- Part 2: How many seconds was the most leading reindeer leading.
--   We represent the progress of each reindeer by the moving sum of its progress over time
--   which is represented by an infinite repetition of speed*[fly] followed by rest*[0].

import Shared (splitString)

type Reindeer = (Int, Int, Int)

parseReindeer :: String -> [Reindeer]
parseReindeer input = map parseLine (lines input)
  where
    parseLine :: String -> Reindeer
    parseLine line = (speed, fly, rest)
      where
        frags = splitString (==' ') line
        speed = read $ frags !! 3
        fly = read $ frags !! 6
        rest = read $ frags !! 13

part1 :: [Reindeer] -> Int
part1 reindeer = maximum $ map (race 2503) reindeer
  where
    race :: Int -> Reindeer -> Int
    race nSecs (speed, fly, rest) = speed*(fly*nBlocks + remaining)
      where
        blockSize = fly+rest
        nBlocks   = nSecs `div` blockSize
        remaining = minimum [nSecs `mod` blockSize, fly]

part2 :: [Reindeer] -> Int
part2 reindeer = race 2503 (map perSecond reindeer) startingPoints
  where
    race :: Int -> [[Int]] -> [Int] -> Int
    race 0 _ scores     = maximum scores
    race nSecs progress scores = race (nSecs - 1) progress' scores'
      where
        progress'  = map (drop 1) progress
        scores'    = zipWith (+) scores $ awardPoints $ map (!! 0) progress
    -- perSecond returns an infinite list that per second reports the distance covered by this reindeer
    perSecond :: Reindeer -> [Int]
    perSecond (speed, fly, rest) = scanl1 (+) $ cycle $ ((take fly (repeat speed)) ++ (take rest (repeat 0)))

    startingPoints :: [Int]
    startingPoints = take (length reindeer) $ repeat 0
    -- awardPoints assigns a single point for each reindeer with the max progress
    awardPoints :: [Int] -> [Int]
    awardPoints progress = map (\dist -> if dist == max then 1 else 0) progress
      where
        max = maximum progress



day14 :: String -> (String, Int, Int)
day14 input = do
  let reindeer = parseReindeer input
  ("Day 14: Reindeer Olympics", part1 reindeer, part2 reindeer)
