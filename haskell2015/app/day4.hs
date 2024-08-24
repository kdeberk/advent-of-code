module Day4 where

-- 2015, Day 4
-- Mint AdventCoins by finding a number n so that hash of "<n><input>" starts with a certain number of zeroes.
--
-- Part 1: Find a number that leads to a hash with 5 leading zeroes.
-- Part 2: Find a number that leads to a hash with 6 leading zeroes.

import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (pack, unpack)
import Data.List (find, isPrefixOf)

import Shared (unwrap)

digest :: String -> String
digest = unpack . encode . MD5.hash . pack

findHashWithPrefix :: String -> String -> Int
findHashWithPrefix secret prefix = unwrap (find (hasPrefix . mine) [0..])
  where
    hasPrefix :: String -> Bool
    hasPrefix = isPrefixOf prefix
    mine :: Int -> String
    mine n = digest (secret ++ (show n))

part1 :: String -> Int
part1 secret = findHashWithPrefix secret "00000"

part2 :: String -> Int
part2 secret = findHashWithPrefix secret "000000"

day4 :: String -> (String, Int, Int)
day4 input = do
  let secret = (lines input) !! 0
  ("Day 4: The Ideal Stocking Stuffer", part1 secret, 0) -- Part 2 is slow
