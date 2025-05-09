module Shared where

import Text.Read (readMaybe)
import Data.Maybe (fromJust)
import qualified Data.List as L
import qualified Data.Set as S

-- splitString breaks up a string based on a single char and discards the char.
splitString :: (Char -> Bool) -> String -> [String]
splitString pred str = case dropWhile pred str of
                        "" -> []
                        s' -> word : splitString pred s''   -- recurse on rest of words
                          where (word, s'') = break pred s' -- break str once using pred

-- breakString breaks up a string based on a single char, and keeps the char.
breakString :: (Eq a) => (a -> Bool) -> [a] -> [[a]]
breakString pred seq
  | idx == Nothing = []
  | otherwise      = [slice 0 (idx'+1) seq] ++ breakString pred (slice (idx'+1) (length seq) seq)
  where
    idx = L.findIndex pred seq
    idx' = fromJust idx

-- splitStringNTimes breaks up a string based on a single char
splitStringNTimes :: (Char -> Bool) -> Int -> String -> [String]
splitStringNTimes _    0 str = [str]
splitStringNTimes pred n str = case dropWhile pred str of
                        "" -> []
                        s' -> word : splitStringNTimes pred (n - 1) s'' -- recurse on rest of words
                          where (word, s'') = break pred s'             -- break str once using pred

-- pairs returns all unique pairs [a,b,c] -> [[a,b],[a,c],[b,c]]
pairs :: [a] -> [] [a]
pairs [] = []
pairs (x : xs) = (pairElement x xs) ++ pairs xs
  where
    pairElement :: a -> [a] -> [] [a]
    pairElement _ [] = []
    pairElement elem (x : xs) = [[elem, x]] ++ pairElement elem xs

-- unwrap unwraps the Just. Throws an error if it's Nothing.
-- perhaps replace with fromJust?
unwrap :: Maybe a -> a
unwrap Nothing = error "Maybe.unwrap: Nothing"
unwrap (Just x) = x

-- isJust returns true if the given value is a Just.
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

allMatch :: [(a -> Bool)] -> a -> Bool
allMatch fns x = all ($ x) fns

-- isNumeric returns true if the string only contains digits
isNumeric :: String -> Bool
isNumeric str = isJust ((readMaybe str)::Maybe Int)

consecutiveTriples :: [a] -> [(a, a, a)]
consecutiveTriples list = zip3 list (tail list) (tail (tail list))

-- slice returns the subsequence starting at index from and up to, but excluding to.
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from) (drop from xs)

indexOf :: (Eq a) => a -> [a] -> Maybe Int
indexOf el xs = inner 0 xs
  where
    inner _ [] = Nothing
    inner idx (x:xs)
      | el == x   = Just idx
      | otherwise = inner (idx+1) xs

indexOfSubsequence :: (Eq a) => [a] -> [a] -> Maybe Int
indexOfSubsequence sub xs = inner 0 xs
  where
    inner _ [] = Nothing
    inner idx xs
      | L.isPrefixOf sub xs = Just idx
      | otherwise           = inner (idx+1) (tail xs)

uniq :: (Ord a) => [a] -> [a]
uniq xs = S.elems $ S.fromList xs
