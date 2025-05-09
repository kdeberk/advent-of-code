
Solutions for the 2015 Advent of Code challenge in Haskell

# How to run

Run and debug a single day with the repl
```
> cabal repl
ghci > day "input/day19.txt" day19
-- reload file with
ghci > :load app/day19.hs
ghci > input <- readFile "input/day19.txt"
-- load all functions into current scope
ghci > :module + *Day19
```

Run all days
```
> cabal run
```
