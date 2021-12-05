module Day02 where

import Data.Function ((&))

type Command = (String, Int)

-- (x, d)
type Position = (Int, Int)

parseLine :: [Char] -> Command
parseLine inputLines = (a, tail b & read :: Int)
  where
    (a, b) = break (== ' ') inputLines

parseInput :: [String] -> [Command]
parseInput = map parseLine

commandToDelta :: Command -> Position
commandToDelta ("forward", amount) = (amount, 0)
commandToDelta ("up", amount) = (0, - amount)
commandToDelta ("down", amount) = (0, amount)
commandToDelta (direction, amount) = error "Invalid command"

processCommands :: [Command] -> Position
processCommands commands =
  commands
    & map commandToDelta
    & foldl (\(x, d) (dx, dd) -> (x + dx, d + dd)) (0, 0)

part1 :: [String] -> String
part1 inputLines =
  x * d & show
  where
    (x, d) =
      inputLines
        & parseInput
        & processCommands
