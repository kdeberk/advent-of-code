module Day15 where

import qualified Data.Map as M
import Data.Map (Map)
import Shared (splitString)

-- 2015, Day 15.

type Ingredient = [Int]



parseIngredient :: String -> Ingredient
parseIngredient line = map (\frag -> read $ last $ splitString (==' ') frag) $ splitString (==',') line

totalScore :: [Ingredient] -> Map Int Int -> Int
totalScore ingredients mapping = product $ map (max 0) $ foldl1 (zipWith (+)) $ M.elems $ M.mapWithKey (\idx count -> map (* count) (ingredients !! idx)) mapping


part1 :: [Ingredient] -> Int
part1 ingredients = inner 0 100 M.empty
  where
    inner :: Int -> Int -> Map Int Int -> Int
    inner idx leftTeaspoons mapping
      | idx == length withoutCalories = totalScore withoutCalories mapping
      | otherwise                     = maximum $ map (\n -> inner (idx + 1) (leftTeaspoons - n) (M.insert idx n mapping)) [0..leftTeaspoons]

    withoutCalories = map init ingredients

part2 :: [Ingredient] -> Int
part2 ingredients = inner 0 100 M.empty
  where
    inner :: Int -> Int -> Map Int Int -> Int
    inner idx leftTeaspoons mapping
      | caloryCount > 500     = 0
      | idx == length withoutCalories = if caloryCount == 500 then (totalScore withoutCalories mapping) else 0
      | otherwise                     = maximum $ map (\n -> inner (idx + 1) (leftTeaspoons - n) (M.insert idx n mapping)) [0..leftTeaspoons]
        where
          caloryCount = sum $ M.elems $ M.mapWithKey (\idx count -> count * last (ingredients !! idx)) mapping

    withoutCalories = map init ingredients


day15 :: String -> (String, Int, Int)
day15 input = do
  let ingredients = map parseIngredient $ lines input
  ("Day 15: Science for Hungry People", part1 ingredients, part2 ingredients)
