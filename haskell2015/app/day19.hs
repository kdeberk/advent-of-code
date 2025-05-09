module Day19 where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Ord as O
import Data.Maybe (fromJust)
import Data.Map ((!))

import Shared (splitString, indexOfSubsequence, breakString, uniq)

type Atom = String
type Sequence = [Atom]
type Mapping = M.Map Sequence [Sequence]
type Range = (Int, Int)

type Puzzle = (Sequence, Mapping)

-- parse a string and break it up into an array of Atoms.
--   "ThCaPB" -> ["Th", "Ca", "P", "B"]
parseSequence :: String -> Sequence
parseSequence "e" = ["e"]
parseSequence str = map reverse $ reverse $ breakString C.isUpper $ reverse str

-- parseMapping produces a mapping between all sequences and their possible replacements.
parseMapping :: [String] -> Mapping
parseMapping lines =
  foldl (\acc (from, to) -> M.insertWith (++) from [to] acc) M.empty mappedSequences
  where
    mappedSequences :: [(Sequence, Sequence)]
    mappedSequences = map (\frags -> (parseSequence (frags !! 0), parseSequence (frags !! 2))) mappedLines
    mappedLines :: [[String]]
    mappedLines = map (\line -> splitString (==' ') line) lines

parseInput :: String -> Puzzle
parseInput input = (sequence, mapping)
  where
    sequence = parseSequence $ last lines
    mapping = parseMapping $ take (length lines - 1) lines
    lines = splitString (== '\n') input

-- findIndices returns all indices at which the subseq appears
findIndices :: (Eq a) => [a] -> [a] -> [Int]
findIndices seq subseq =
  filter (\idx -> L.isPrefixOf subseq (drop idx seq)) [0..(length seq)]

-- at the given index, replace the subsequence with any of the possible alternatives specified by mapping.
replaceAtom :: Mapping -> Sequence -> Sequence -> Int -> [Sequence]
replaceAtom mapping seq subseq idx =
  map (\repl -> prefix ++ repl ++ suffix) $ mapping ! subseq
    where
      prefix = take idx seq
      suffix = drop (idx + length subseq) seq

-- allReplacements generates the list of all possible following sequences
allReplacements :: Mapping -> Sequence -> [Sequence]
allReplacements mapping sequence =
  uniq $ M.keys mapping >>= replaceAllOccurences
    where
      replaceAllOccurences subseq = findIndices sequence subseq >>= replaceAtom mapping sequence subseq

part1 :: Puzzle -> Int
part1 (sequence, mapping) =
  length $ allReplacements mapping sequence

-- reduces the given sequence to its smallest possible reachable state. The important assumption is that the mapping maps larger
--  sequences to smaller otherwise this function will never terminate.
reduce :: Mapping -> Sequence -> (Sequence, Int)
reduce mapping sequence =
  (smallest, seen ! smallest)
    where
      smallest = L.minimumBy (O.comparing length) $ M.keys seen
      seen = inner [(sequence, 0)] M.empty
      -- inner navigates through the set of all possible reachable sequences, obtained by applying the
      --  mapping wherever applicable. If the mapping always maps larger to smaller strings, then this
      --  function will eventually terminate. The resulting will contain all visited sequences that were
      --  reachable via the mapping.
      inner :: [(Sequence, Int)] -> M.Map Sequence Int -> M.Map Sequence Int
      inner queue seen
        | queue == []       = seen
        | M.member cur seen = inner (tail queue) seen
        | otherwise         = inner (tail queue ++ nextSteps) (M.insert cur idx seen)
          where
            (cur, idx) = head queue
            nextSteps :: [(Sequence, Int)]
            nextSteps = map (\seq -> (seq, idx+1)) $ filter isNew reduced
            reduced = allReplacements mapping cur
            isNew :: Sequence -> Bool
            isNew seq = M.notMember seq seen

-- split sequence up into smaller one, each sequence either ending with the specified atom, or be the final sequence.
splitAtTerminalAtom :: Sequence -> Atom -> [Sequence]
splitAtTerminalAtom seq atom
  | idx == Nothing = [seq]
  | otherwise      = [take idx' seq] ++ splitAtTerminalAtom (drop idx' seq) atom
    where
      idx = indexOfSubsequence [atom] seq
      idx' = (fromJust idx) + 1

-- reduce the entire sequence to a smaller sequence, but first break it up into smaller sequences. These sequences
--  need to end with "Ar", because that Atom only appears last, and thus breaking at "Ar" won't disallow
--  valid reductions.
splitAndReduce :: Mapping -> Sequence -> (Sequence, Int)
splitAndReduce mapping seq =
  foldl combine ([], 0) reduced
    where
      combine :: (Sequence, Int) -> (Sequence, Int) -> (Sequence, Int)
      combine (seqa, dsta) (seqb, dstb) = (seqa ++ seqb, dsta + dstb)
      reduced :: [(Sequence, Int)]
      reduced = map (reduce mapping) frags
      frags :: [Sequence]
      frags = splitAtTerminalAtom seq "Ar"

-- keep applying reduce until dst has been reached
reduceTo :: Mapping -> Sequence -> Sequence -> Int
reduceTo mapping seq target
  | reduced == target = dst
  | otherwise         = dst + reduceTo mapping reduced target
    where
      (reduced, dst) = splitAndReduce mapping seq

invertMapping :: Mapping -> Mapping
invertMapping mapping =
  M.fromList $ M.assocs mapping >>= invert
    where
      invert :: (Sequence, [Sequence]) -> [(Sequence, [Sequence])]
      invert (key, elems) = map (\elem -> (elem, [key])) elems

part2 :: Puzzle -> Int
part2 (sequence, mapping) =
  reduceTo (invertMapping mapping) sequence ["e"]

day19 :: String -> (String, Int, Int)
day19 input = do
  let puzzle = parseInput input
  ("Day 19: Medicine for Rudolph", part1 puzzle, part2 puzzle)
