module Day13 where

-- 2015, Day 13.
-- We iterate over permutations of a set and for each permutation determine the best score.
-- Part 1: Set of 8 people.
-- Part 2: Set of 9 people.

import qualified Data.Map as M
import qualified Data.Set as S
import Shared (splitString)

type Table = (Attendees, Relations)
type Attendee = String
type Attendees = S.Set Attendee
type Relations = M.Map (Attendee, Attendee) Int


parseTable :: String -> Table
parseTable input = buildTable (lines input) S.empty M.empty
  where
    buildTable :: [String] -> Attendees -> Relations -> Table
    buildTable [] attendees relations = (attendees, relations)
    buildTable (line:xs) attendees relations = do
      let frags = splitString (==' ') line
      let personA = frags !! 0
      let personB = init $ frags !! 10
      let change = case (frags !! 2) of
                    "gain" -> 1
                    "lose" -> -1
      let units = change * (read (frags !! 3))
      buildTable xs
        (foldr S.insert attendees [personA, personB])
        (M.insert (personA, personB) units relations)

findOptimalSeating :: Table -> Int
findOptimalSeating (attendees, relations) = inner attendees []
  where
    inner :: Attendees -> [Attendee] -> Int
    inner attendees order
      | S.size attendees == 0 = totalHappiness order
      | otherwise = maximum $ S.map (\next -> inner (S.delete next attendees) (order++[next])) attendees

    totalHappiness :: [Attendee] -> Int
    totalHappiness order = sum $ map happiness $ allPairs order
    allPairs :: [Attendee] -> [(Attendee, Attendee)]
    allPairs order = foldl (++) [] $ map (\(a, b) -> [(a,b), (b,a)]) $ zip order $ drop 1 $ cycle order
    happiness :: (Attendee, Attendee) -> Int
    happiness pair = case (M.lookup pair relations) of
      Nothing -> 0
      Just n -> n

part1 :: Table -> Int
part1 table = findOptimalSeating $ table

part2 :: Table -> Int
part2 (attendees, relations) = findOptimalSeating (S.insert "yourself" attendees, relations)

day13 :: String -> (String, Int, Int)
day13 input = do
  let table = parseTable input
  ("Day 13: Knights of the Dinner Table", part1 table, part2 table)
