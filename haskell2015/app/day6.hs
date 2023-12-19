module Day6 where

type Range = ((Int, Int), (Int, Int))
data Command = Toggle Range | TurnOn Range | TurnOff Range

parseLine :: String -> Command
parseLine input
  | isPrefixOf "toggle" input = Toggle frags!!0
  | isPrefixOf "turn on" input = parseTurnOnLine input
  | isPrefixOf "turn off" input = parseTurnOffLine input
  where
    frags = (\ ch -> ch == " " || ch == ",")





day6 :: String -> (Int, Int)
day6 input = do
  let
