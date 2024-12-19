module Shared where

import Text.Read (readMaybe)

-- splitString breaks up a string based on a single char and discards the char.
splitString :: (Char -> Bool) -> String -> [String]
splitString pred str = case dropWhile pred str of
                        "" -> []
                        s' -> word : splitString pred s''   -- recurse on rest of words
                          where (word, s'') = break pred s' -- break str once using pred

-- breakString breaks up a string based on a single char, and keeps the char.
breakString :: (Char -> Bool) -> String -> [String]
breakString pred str = case str of
                        "" -> []
                        s' -> word : breakString pred s''   -- recurse on rest of words
                          where (word, s'') = break pred s' -- break str once using pred


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
