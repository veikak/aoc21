module Day02 where

import Data.Function ((&))

type Command = (String, Int)

-- (x, d)
type Position = (Int, Int)

-- (x, d, a)
type Position2 = (Int, Int, Int)

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

commandToDelta2 :: Command -> Int -> Position2
commandToDelta2 ("forward", amount) aim = (amount, aim * amount, 0)
commandToDelta2 ("up", amount) aim = (0, 0, - amount)
commandToDelta2 ("down", amount) aim = (0, 0, amount)
commandToDelta2 (direction, amount) aim = error "Invalid command"

processCommands :: [Command] -> Position
processCommands commands =
  commands
    & map commandToDelta
    & foldl (\(x, d) (dx, dd) -> (x + dx, d + dd)) (0, 0)

processCommand2 :: Position2 -> Command -> Position2
processCommand2 (x, d, a) command = (x + dx, d + dd, a + da)
  where (dx, dd, da) = commandToDelta2 command a

processCommands2 :: [Command] -> Position2
processCommands2 = foldl processCommand2 (0, 0, 0)

part1 :: [String] -> String
part1 inputLines =
  x * d & show
  where
    (x, d) =
      inputLines
        & parseInput
        & processCommands

part2 :: [String] -> String
part2 inputLines =
  x * d & show
  where
    (x, d, a) =
      inputLines
        & parseInput
        & processCommands2
