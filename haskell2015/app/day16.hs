{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-incomplete-uni-patterns #-}

module Day16 where

-- 2015, Day 16.

import Shared (splitString, splitStringNTimes, unwrap)
import qualified Data.Map as M

import Data.Map (Map, (!))

type Sue = (Int, Map String Int)

-- parseSue takes a "Sue X: foo: N, bar: M, baz: Z, .." and returns the tuple containing
--  X and a the mapping of the keys to numbers.
parseSue :: String -> Sue
parseSue line = do
  let [sue, rest] = splitStringNTimes (==':') 1 line
  (read $ drop 4 sue, M.fromList $ map makeEntry $ splitString (==',') rest)
    where
      makeEntry :: String -> (String, Int)
      makeEntry str = (drop 1 field, read count)
        where
          [field, count] = splitString (==':') str

tickerTape :: Map String Int
tickerTape = M.fromList [
  ("children", 3), ("cats", 7), ("samoyeds", 2), ("pomeranians", 3), ("akitas", 0), ("vizslas", 0), ("goldfish", 5), ("trees", 3), ("cars", 2), ("perfumes", 1)]

findSue :: (String -> Int -> Int -> Bool) -> [Sue] -> Maybe Sue
findSue _ [] = Nothing
findSue match (sue:xs)
  | all matchValue $ M.keys tickerTape = Just sue
  | otherwise = findSue match xs
    where
      matchValue :: String -> Bool
      matchValue key = case (M.lookup key m) of
                         Nothing -> True
                         Just value -> match key value (tickerTape ! key)
      (_, m) = sue

part1 :: [Sue] -> Int
part1 sues = found
  where
    (found, _) = unwrap $ findSue matchKey sues
    matchKey :: String -> Int -> Int -> Bool
    matchKey _ remembered detected = remembered == detected

part2 :: [Sue] -> Int
part2 sues = found
  where
    (found, _) = unwrap $ findSue matchKey sues
    matchKey :: String -> Int -> Int -> Bool
    matchKey "cats" remembered detected = remembered > detected
    matchKey "trees" remembered detected = remembered > detected
    matchKey "pomeranians" remembered detected = remembered < detected
    matchKey "goldfish" remembered detected = remembered < detected
    matchKey _ remembered detected = remembered == detected

day16 :: String -> (String, Int, Int)
day16 input = do
  let sues = map parseSue $ lines input
  ("Day 16: Aunt Sue", part1 sues, part2 sues)
