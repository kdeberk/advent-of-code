module Shared where

-- runDay applies a day function to an input file.
runDay :: Show a => Int -> String -> (String -> a) -> IO ()
runDay n filename dayFn = do
    contents <- readFile filename
    let parsed = dayFn contents
    print $ "Day " ++ (show n) ++ " " ++ (show parsed)

-- splitWords breaks up a string based on a single char
splitWords :: (Char -> Bool) -> String -> [String]
splitWords pred str = case dropWhile pred str of
                        "" -> []
                        s' -> word : splitWords pred s''    -- recurse on rest of words
                          where (word, s'') = break pred s' -- break str once using pred

-- pairs returns all unique pairs [a,b,c] -> [[a,b],[a,c],[b,c]]
pairs :: [a] -> [] [a]
pairs [] = []
pairs (x : xs) = (pairElement x xs) ++ pairs xs
  where
    pairElement :: a -> [a] -> [] [a]
    pairElement _ [] = []
    pairElement elem (x : xs) = [[elem, x]] ++ pairElement elem xs

fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

allMatch :: [(a -> Bool)] -> a -> Bool
allMatch fns x = all ($ x) fns
