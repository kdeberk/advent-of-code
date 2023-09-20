part1 :: String -> Int
part1 x = inner x 0
      where
        inner :: String -> Int -> Int
        inner "" floor = floor
        inner ('(' : x) floor = inner x (floor + 1)
        inner (')' : x) floor = inner x (floor - 1)

part2 :: String -> Int
part2 x = inner x 0 0
      where
        inner :: String -> Int -> Int -> Int
        -- We've reached floor -1, return idx.
        inner _ (-1) idx = idx
        inner ('(' : x) floor idx = inner x (floor + 1) (idx + 1)
        inner (')' : x) floor idx = inner x (floor - 1) (idx + 1)

day1 :: [String] -> (Int, Int)
day1 lines = do
     let [line] = lines
     (part1 line, part2 line)

-- readInput applies a day function to an input file.
readInput :: String -> ([String] -> a) -> IO a
readInput filename dayFn = do
    contents <- readFile filename
    let parsed = dayFn (lines contents)
    return parsed
