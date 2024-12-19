module Day19 where

import qualified Data.Char as C

type Atom = Char

parseMolecule :: [Char] -> String -> [[Char]]
parseMolecule cs "" = [cs]
parseMolecule cs (x:xs) =
  if cs == [] && C.toUpper(x) == x then
    parseMolecule [x] xs
  else if C.toUpper(x) == x then
    [cs] ++ parseMolecule [x] xs
  else
    parseMolecule (cs ++ [x]) xs
