{-# LANGUAGE ScopedTypeVariables #-}

module Day7 where

-- 2015, Day 7
-- The input consists of several bit-pattern carrying wires that are connected to each other with
---  logic gates (AND, OR, NOT, RSHIFT and LSHIFT).
-- The wires are presented out of order and form a DAG. To obtain the value of one wire,
--  you often have to evaluate the values of some other wires first. This recursive algorithm terminates
--  as all wires eventually refer to wires that are assigned literal values and do not refer to other wires.
--
-- Part 1: What's the value of wire 'a'?
-- Part 2: Set wire 'b' to the previous value of wire 'a' and rerun. Now what is the value of wire 'a'?


import Data.Bits ((.&.), (.|.), shiftL, shiftR, complement)
import qualified Data.Map as M
import qualified Data.Word as W
import Shared (isNumeric, splitWords, isJust)
import Data.Map ((!))

data Instruction =
  Assign String
  | And String String
  | Or String String
  | LShift String Int
  | RShift String Int
  | Not String
  deriving Show

type Instructions = M.Map Wire Instruction
type Circuit = M.Map Wire W.Word16
type Wire = String

gatherInstructions :: [String] -> Instructions
gatherInstructions lines =
  foldl (\acc (reg, ins) -> M.insert reg ins acc) M.empty (map parseLine lines)
  where
    parseLine :: String -> (String, Instruction)
    parseLine line =
      case (splitWords (==' ') line) of
        [signal, "->", reg] -> (reg, Assign signal)
        [w1, "AND", w2, "->", reg] -> (reg, And w1 w2)
        [w1, "OR", w2, "->", reg] -> (reg, Or w1 w2)
        [w1, "LSHIFT", n, "->", reg] -> (reg, LShift w1 (read n))
        [w1, "RSHIFT", n, "->", reg] -> (reg, RShift w1 (read n))
        ["NOT", w, "->", reg] -> (reg, Not w)

resolveValue :: Circuit -> Instructions -> Wire -> Int
resolveValue circuit ins wire = fromIntegral ((resolve circuit wire) ! wire)
  where
    -- resolve returns a circuit in which the value of the specified wire has been resolved.
    resolve :: Circuit -> Wire -> Circuit
    resolve circuit wire
      -- Wire value has already been determined
      | isJust (M.lookup wire circuit) = circuit
      -- Wire value is an integer literal. No further searching needed,
      | isNumeric wire = M.insert wire (read wire) circuit
      | otherwise =
          case (ins ! wire) of
            Assign val    -> let circuit' = resolve circuit val
                              in M.insert wire (circuit' ! val) circuit'
            And val1 val2 -> let circuit' = foldl resolve circuit [val1, val2]
                              in M.insert wire ((circuit' ! val1) .&. (circuit' ! val2)) circuit'
            Or val1 val2  -> let circuit' = foldl resolve circuit [val1, val2]
                              in M.insert wire ((circuit' ! val1) .|. (circuit' ! val2)) circuit'
            LShift val n  -> let circuit' = resolve circuit val
                              in M.insert wire ((circuit' ! val) `shiftL` n) circuit'
            RShift val n  -> let circuit' = resolve circuit val
                              in M.insert wire ((circuit' ! val) `shiftR` n) circuit'
            Not val       -> let circuit' = resolve circuit val
                              in M.insert wire (complement (circuit' ! val)) circuit'

part1 :: Instructions -> Int
part1 instructions = resolveValue M.empty instructions "a"

part2 :: Instructions -> Int
part2 instructions = resolveValue withB instructions "a"
  where
    a = resolveValue M.empty instructions "a"
    withB = M.insert "b" (fromIntegral a) M.empty


day7 :: String -> (String, Int, Int)
day7 input = do
  let instructions = gatherInstructions (lines input)
  ("Day 7: Some Assembly Required", part1 instructions, part2 instructions)
