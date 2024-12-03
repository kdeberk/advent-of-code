module Day10 where

-- 2015, Day 10
-- The input is a seed for the Look-and-Say game as described by Conway. We use his 'audio-active' decay
--  concept to describe the nth generation not as a sequence of digits, but as a collection of
--  elements that the previous generation decayed into.
--
-- Part 1: What is the length of the sequence after 40 iterations.
-- Part 2: What is the length of the sequence after 50 iterations.

import qualified Data.Map as M
import Data.Map (Map, (!))

-- lookAndSay_naive generates the sequence by generating the entire sequence.
-- To obtain the nth item, call (iterate lookAndSay_naive start) !! n
lookAndSay_naive :: [Int] -> [Int]
lookAndSay_naive digits = flatten (reverse (foldl inner [] digits))
  where
    inner :: [(Int, Int)] -> Int -> [(Int, Int)]
    inner [] cur = [(1, cur)]
    inner ((count, cur) : rest) nxt
      | nxt == cur = [(count + 1, cur)] ++ rest
      | otherwise = [(1, nxt), (count, cur)] ++ rest
    flatten :: [(Int, Int)] -> [Int]
    flatten [] = []
    flatten ((cur, count) : rest) = [cur, count] ++ flatten rest

-- Conway noticed that the strings generated by applying 'look-and-say' sequence can be seen as collections
--  of 'atomic elements' that decay into other elements after each generation. The following map
--  contains the 92 elements he discovered, along with the strings they represent and what elements they decay into.
-- This list was mostly generated by taking the table from https://en.wikipedia.org/wiki/Look-and-say_sequence
--  and executing 'cat <file> | cut -d$'\t' -f 2-4 | gsed 's/\./",1),("/g' | awk '{print "  (\""$1"\", (\""$2"\", M.fromList [(\""$3"\",1)])),"}'
elements :: Map String (String, Map String Int)
elements = M.fromList [
  ("H", ("22", M.fromList [("H",1)])),
  ("He", ("13112221133211322112211213322112", M.fromList [("Hf",1),("Pa",1),("H",1),("Ca",1),("Li",1)])),
  ("Li", ("312211322212221121123222112", M.fromList [("He",1)])),
  ("Be", ("111312211312113221133211322112211213322112", M.fromList [("Ge",1),("Ca",1),("Li",1)])),
  ("B", ("1321132122211322212221121123222112", M.fromList [("Be",1)])),
  ("C", ("3113112211322112211213322112", M.fromList [("B",1)])),
  ("N", ("111312212221121123222112", M.fromList [("C",1)])),
  ("O", ("132112211213322112", M.fromList [("N",1)])),
  ("F", ("31121123222112", M.fromList [("O",1)])),
  ("Ne", ("111213322112", M.fromList [("F",1)])),
  ("Na", ("123222112", M.fromList [("Ne",1)])),
  ("Mg", ("3113322112", M.fromList [("Pm",1),("Na",1)])),
  ("Al", ("1113222112", M.fromList [("Mg",1)])),
  ("Si", ("1322112", M.fromList [("Al",1)])),
  ("P", ("311311222112", M.fromList [("Ho",1),("Si",1)])),
  ("S", ("1113122112", M.fromList [("P",1)])),
  ("Cl", ("132112", M.fromList [("S",1)])),
  ("Ar", ("3112", M.fromList [("Cl",1)])),
  ("K", ("1112", M.fromList [("Ar",1)])),
  ("Ca", ("12", M.fromList [("K",1)])),
  ("Sc", ("3113112221133112", M.fromList [("Ho",1),("Pa",1),("H",1),("Ca",1),("Co",1)])),
  ("Ti", ("11131221131112", M.fromList [("Sc",1)])),
  ("V", ("13211312", M.fromList [("Ti",1)])),
  ("Cr", ("31132", M.fromList [("V",1)])),
  ("Mn", ("111311222112", M.fromList [("Cr",1),("Si",1)])),
  ("Fe", ("13122112", M.fromList [("Mn",1)])),
  ("Co", ("32112", M.fromList [("Fe",1)])),
  ("Ni", ("11133112", M.fromList [("Zn",1),("Co",1)])),
  ("Cu", ("131112", M.fromList [("Ni",1)])),
  ("Zn", ("312", M.fromList [("Cu",1)])),
  ("Ga", ("13221133122211332", M.fromList [("Eu",1),("Ca",2),("Ac",1),("H",1),("Zn",1)])),
  ("Ge", ("31131122211311122113222", M.fromList [("Ho",1),("Ga",1)])),
  ("As", ("11131221131211322113322112", M.fromList [("Ge",1),("Na",1)])),
  ("Se", ("13211321222113222112", M.fromList [("As",1)])),
  ("Br", ("3113112211322112", M.fromList [("Se",1)])),
  ("Kr", ("11131221222112", M.fromList [("Br",1)])),
  ("Rb", ("1321122112", M.fromList [("Kr",1)])),
  ("Sr", ("3112112", M.fromList [("Rb",1)])),
  ("Y", ("1112133", M.fromList [("Sr",1),("U",1)])),
  ("Zr", ("12322211331222113112211", M.fromList [("Y",1),("H",1),("Ca",1),("Tc",1)])),
  ("Nb", ("1113122113322113111221131221", M.fromList [("Er",1),("Zr",1)])),
  ("Mo", ("13211322211312113211", M.fromList [("Nb",1)])),
  ("Tc", ("311322113212221", M.fromList [("Mo",1)])),
  ("Ru", ("132211331222113112211", M.fromList [("Eu",1),("Ca",1),("Tc",1)])),
  ("Rh", ("311311222113111221131221", M.fromList [("Ho",1),("Ru",1)])),
  ("Pd", ("111312211312113211", M.fromList [("Rh",1)])),
  ("Ag", ("132113212221", M.fromList [("Pd",1)])),
  ("Cd", ("3113112211", M.fromList [("Ag",1)])),
  ("In", ("11131221", M.fromList [("Cd",1)])),
  ("Sn", ("13211", M.fromList [("In",1)])),
  ("Sb", ("3112221", M.fromList [("Pm",1),("Sn",1)])),
  ("Te", ("1322113312211", M.fromList [("Eu",1),("Ca",1),("Sb",1)])),
  ("I", ("311311222113111221", M.fromList [("Ho",1),("Te",1)])),
  ("Xe", ("11131221131211", M.fromList [("I",1)])),
  ("Cs", ("13211321", M.fromList [("Xe",1)])),
  ("Ba", ("311311", M.fromList [("Cs",1)])),
  ("La", ("11131", M.fromList [("Ba",1)])),
  ("Ce", ("1321133112", M.fromList [("La",1),("H",1),("Ca",1),("Co",1)])),
  ("Pr", ("31131112", M.fromList [("Ce",1)])),
  ("Nd", ("111312", M.fromList [("Pr",1)])),
  ("Pm", ("132", M.fromList [("Nd",1)])),
  ("Sm", ("311332", M.fromList [("Pm",1),("Ca",1),("Zn",1)])),
  ("Eu", ("1113222", M.fromList [("Sm",1)])),
  ("Gd", ("13221133112", M.fromList [("Eu",1),("Ca",1),("Co",1)])),
  ("Tb", ("3113112221131112", M.fromList [("Ho",1),("Gd",1)])),
  ("Dy", ("111312211312", M.fromList [("Tb",1)])),
  ("Ho", ("1321132", M.fromList [("Dy",1)])),
  ("Er", ("311311222", M.fromList [("Ho",1),("Pm",1)])),
  ("Tm", ("11131221133112", M.fromList [("Er",1),("Ca",1),("Co",1)])),
  ("Yb", ("1321131112", M.fromList [("Tm",1)])),
  ("Lu", ("311312", M.fromList [("Yb",1)])),
  ("Hf", ("11132", M.fromList [("Lu",1)])),
  ("Ta", ("13112221133211322112211213322113", M.fromList [("Hf",1),("Pa",1),("H",1),("Ca",1),("W",1)])),
  ("W", ("312211322212221121123222113", M.fromList [("Ta",1)])),
  ("Re", ("111312211312113221133211322112211213322113", M.fromList [("Ge",1),("Ca",1),("W",1)])),
  ("Os", ("1321132122211322212221121123222113", M.fromList [("Re",1)])),
  ("Ir", ("3113112211322112211213322113", M.fromList [("Os",1)])),
  ("Pt", ("111312212221121123222113", M.fromList [("Ir",1)])),
  ("Au", ("132112211213322113", M.fromList [("Pt",1)])),
  ("Hg", ("31121123222113", M.fromList [("Au",1)])),
  ("Tl", ("111213322113", M.fromList [("Hg",1)])),
  ("Pb", ("123222113", M.fromList [("Tl",1)])),
  ("Bi", ("3113322113", M.fromList [("Pm",1),("Pb",1)])),
  ("Po", ("1113222113", M.fromList [("Bi",1)])),
  ("At", ("1322113", M.fromList [("Po",1)])),
  ("Rn", ("311311222113", M.fromList [("Ho",1),("At",1)])),
  ("Fr", ("1113122113", M.fromList [("Rn",1)])),
  ("Ra", ("132113", M.fromList [("Fr",1)])),
  ("Ac", ("3113", M.fromList [("Ra",1)])),
  ("Th", ("1113", M.fromList [("Ac",1)])),
  ("Pa", ("13", M.fromList [("Th",1)])),
  ("U", ("3", M.fromList [("Pa",1)]))
  ]

-- lookAndSay uses the above elements table to keep track of how many times a certain element exists at a certain iteration.
-- For each iteration, all elements are replaced with the elements they 'decay' into while keeping track of the correct counts.
lookAndSay :: Int -> String -> Int
lookAndSay n str = finalLength (loop n (M.fromList [(start, 1)]))
  where
    start = M.keys (M.filter (\(el, _) -> el == str) elements) !! 0
    -- loop applies the lookAndSay generation step n times.
    loop :: Int -> (Map String Int) -> (Map String Int)
    loop 0 elCounts = elCounts
    loop n elCounts = loop (n-1) (foldl (\acc (el, count) ->
                                            let (_, counts) = elements ! el
                                            in M.unionWith (+) acc (M.map (*count) counts)
                                        ) M.empty (M.assocs elCounts))
    -- finalLength sums the length of each element multiplied by its count.
    finalLength :: Map String Int -> Int
    finalLength elCounts = sum (
      map (\(el, count) -> let (str, _) = elements ! el
                           in count * (length (str)))
        (M.assocs (elCounts))
      )

part1 :: String -> Int
part1 start = lookAndSay 40 start

part2 :: String -> Int
part2 start = lookAndSay 50 start

day10 :: String -> (String, Int, Int)
day10 input = do
  let start = (lines input) !! 0
  ("Day 10: Elves Look, Elves Say", part1 start, part2 start)