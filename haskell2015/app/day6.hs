{-# LANGUAGE RankNTypes #-}

module Day6 where

-- 2015, Day 6
-- Input is a list of instructions for switching lights on, off or toggling them. These
--  lights have integer values. The lights are arranged in a 1000x1000 grid and the instructions
--  apply to rectangles covering lights within this grid.
--
-- Part 1: Lights can be turned on, off or toggled. The lights can only be turned off (0) or on (1).
-- Part 2: Lights have integer values denoting intensity. Turning on and off means
--   incrementing or decrementing. Toggling means incrementing by 2.

import Control.Monad (forM_)
import Control.Monad.ST (ST)
import qualified Data.Array.Unboxed as U
import qualified Data.Array.ST as ST
import qualified Data.Array.MArray as M
import Shared (splitWords)

type Light = Int
type Coord = (Int, Int)
type Rect = (Coord, Coord)
type Grid = U.UArray Coord Light
type UpdateFn = Light -> Light
type Instruction = (Rect, UpdateFn)

-- parseInstructions converts the sequence of lines to Instruction values.
parseInstructions :: [String] -> UpdateFn -> UpdateFn -> UpdateFn -> [Instruction]
parseInstructions lines turnOn turnOff toggle =
  map (\line -> parseFrags (splitWords (==' ') line)) lines
  where
    parseFrags :: [String] -> Instruction
    parseFrags ["toggle", c1, "through", c2] = ((readRect c1 c2), toggle)
    parseFrags ["turn", "on", c1, "through", c2] = ((readRect c1 c2), turnOn)
    parseFrags ["turn", "off", c1, "through", c2] = ((readRect c1 c2), turnOff)
    -- readRect reads a pair of coords
    readRect :: String -> String -> Rect
    readRect c1 c2 = (readCoord c1, readCoord c2)
    -- readCoord parses string "N,M" to tuple (N, M)
    readCoord :: String -> Coord
    readCoord str = (x, y)
      where
        [x, y] = map read (splitWords (==',') str)

-- constructGrid takes the instruction and returns the sum of all light values.
constructGrid :: [Instruction] -> Int
constructGrid commands = foldl (+) 0 (U.elems finalGrid)
  where
    finalGrid = foldl applyInstruction emptyGrid commands
    emptyGrid = ST.runSTUArray $ do
      arr <- ST.newArray ((0, 0), (999, 999)) 0 :: forall s. ST s (ST.STUArray s Coord Light)
      return arr
    -- Applies the command to the grid
    applyInstruction :: Grid -> Instruction -> Grid
    applyInstruction g (r, op) = writeRect g r op
    -- Takes an 'immutable' grid and mutates it by applying the fn for each coord in the rect.
    writeRect :: Grid -> Rect -> (Light -> Light) -> Grid
    writeRect g r fn = ST.runSTUArray $ do
      g' <- ST.thaw g :: forall s. ST s (ST.STUArray s Coord Light)
      forM_ (points r) $ \c -> do
        M.modifyArray g' c fn
      return g'
    -- returns a sequence of all coords within the Rect
    points :: Rect -> [Coord]
    points ((x1, y1), (x2, y2)) = [(x, y) | x <- [x1..x2], y <- [y1..y2]]

part1 :: [String] -> Int
part1 lines = constructGrid (parseInstructions lines turnOn turnOff toggle)
  where
    turnOn _ = 1
    turnOff _ = 0
    toggle 1 = 0
    toggle _ = 1

part2 :: [String] -> Int
part2 lines = constructGrid (parseInstructions lines turnOn turnOff toggle)
  where
    turnOn n = n + 1
    turnOff 0 = 0
    turnOff n = n - 1
    toggle n = n + 2

day6 :: String -> (String, Int, Int)
day6 input = do
  let strings = (lines input)
  ("Day 6: Probably a Fire Hazard", part1 strings, part2 strings)
