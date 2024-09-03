module Day12 where

-- 2015, Day 12.
-- A series of JSON documents contain numbers. Walk through them and sum all numbers. We use Parsec to
--  parse the JSON strings.
-- Part 1: Walk through all lists and objects and all numbers that are contained within them.
-- Part 2: Ignore the numbers that are nested within objects that have a key value pair with "red" as value.

import qualified Text.Parsec as P
import qualified Data.Map as M
import Data.Map (Map)

data JSON = JSONString String | JSONNumber Int | JSONObject (Map String JSON) | JSONList [JSON]
  deriving (Eq, Show)

negNumber :: P.Parsec String () JSON
negNumber = do
  _ <- P.char '-'
  num <- P.many1 P.digit
  return (JSONNumber (-1 * (read num)))

posNumber :: P.Parsec String () JSON
posNumber = do
  num <- P.many1 P.digit
  return (JSONNumber (read num))

number :: P.Parsec String () JSON
number = do
  num <- P.choice [negNumber, posNumber]
  return num

rawString :: P.Parsec String () String
rawString = do
  _ <- P.char '"'
  str <- P.many (P.noneOf("\""))
  _ <- P.char '"'
  return str

jsonString :: P.Parsec String () JSON
jsonString = do
  str <- rawString
  return (JSONString str)

list :: P.Parsec String () JSON
list = do
  _ <- P.char '['
  ns <- P.sepBy json (P.char ',')
  _ <- P.char ']'
  return (JSONList ns)

keyValuePair :: P.Parsec String () (String, JSON)
keyValuePair = do
  key <- rawString
  _ <- P.char ':'
  value <- json
  return (key, value)

object :: P.Parsec String () JSON
object = do
  _ <- P.char '{'
  pairs <- P.sepBy keyValuePair (P.char ',')
  _ <- P.char '}'
  return (JSONObject (M.fromList(pairs)))

json :: P.Parsec String () JSON
json = do
  j <- P.choice [number, jsonString, list, object]
  return j

parse :: String -> JSON
parse text =
  case (P.parse json "(json)" text) of
    Right result -> result
    Left err -> error (show err)


part1 :: String -> Int
part1 str = countNumbers (parse str)
  where
    countNumbers :: JSON -> Int
    countNumbers (JSONNumber n) = n
    countNumbers (JSONList lst) = sum $ map countNumbers lst
    countNumbers (JSONObject obj) = sum $ map countNumbers $ M.elems obj
    countNumbers _ = 0

part2 :: String -> Int
part2 str = countNumbers (parse str)
  where
    countNumbers :: JSON -> Int
    countNumbers (JSONNumber n) = n
    countNumbers (JSONList lst) = sum $ map countNumbers lst
    countNumbers (JSONObject obj)
      | (JSONString "red") `elem` (M.elems obj) = 0
      | otherwise                = sum $ map countNumbers $ M.elems obj
    countNumbers _ = 0

day12 :: String -> (String, Int, Int)
day12 input = do
  let json = (lines input) !! 0
  ("Day 12: JSAbacusFramework.io", part1 json, part2 json)
