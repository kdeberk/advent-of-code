module Day11 where

-- 2015, Day 11.
-- Given an 'expired' password, find the next valid one in the sequence. For most passwords, we can most likely
--  obtain the next one by only changing the last 5 characters. Within this sequence, we satisfy the 1st and 3rd
--  requirement with the sequence aabcc.
-- Part 1: Get the next valid password after the seed.
-- Part 2: Get the next valid password after that one.

import Data.List

import Shared (consecutiveTriples)

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz"

-- suffixes generates the final parts of the strings that are nearly always enough
--  to produce the next item in the password sequence.
suffixes :: [String]
suffixes = map expand $ filter (not . iol) straights
  where
    straights :: [(Char, Char, Char)]
    straights = consecutiveTriples alphabet
    -- iol returns true if the tuple contains i, o or l.
    iol :: (Char, Char, Char) -> Bool
    iol (a, b, c) = any id [x==y | x <- [a,b,c], y <- "iol"]
    expand :: (Char, Char, Char) -> [Char]
    expand (a, b, c) = [a,a,b,c,c]

-- nextSequence returns the first character sequence that follows the given one.
nextSequence :: String -> String
nextSequence str = reverse $ inner $ reverse str
  where
    inner :: [Char] -> [Char]
    inner "" = ""
    inner (x:xs) =
      case nextChar x of
        Just x' -> [x'] ++ xs
        Nothing -> ['a'] ++ inner xs
    nextChar :: Char -> Maybe Char
    nextChar chr =
      case (findIndex (==chr) alphabet) of
        Just 25 -> Nothing
        Just idx -> Just (alphabet !! (idx+1))

-- nextPassword returns a password sequence that is - for all practical purposes -
--  the next valid password. This is done by only modifying the last 5 (or 6)
--  characters in the string.
nextPassword :: String -> String
nextPassword start =
  case suffix of
    Just found
      | found == (drop 3 start) -> (nextSequence prefix) ++ suffixes !! 0
      | otherwise -> prefix ++ found
    Nothing -> (nextSequence prefix) ++ suffixes !! 0
  where
    prefix :: String
    prefix = take 3 start
    suffix :: Maybe String
    suffix = find (isPrefixOf startChar) suffixes
    startChar :: [Char]
    startChar = take 1 $ drop 3 start

part1 :: String -> String
part1 start = nextPassword start

part2 :: String -> String
part2 start = nextPassword $ nextPassword start

day11 :: String -> (String, String, String)
day11 input = do
  let start = (lines input) !! 0
  ("Day 11: Corporate Policy", part1 start, part2 start)
