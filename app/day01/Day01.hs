module Day01 where

import Data.Function ((&))
import Data.List

parseInput :: [String] -> [Int]
parseInput = map read

getDiffs :: Num c => [c] -> [c]
getDiffs depths = zipWith (-) (tail depths) depths

slideWindow :: Num b => [b] -> [b]
slideWindow depths =
  transpose [depths & tail & tail, depths & tail & init, depths & init & init]
    & map sum

countPositive :: [Int] -> Int
countPositive = length . filter (> 0)

part1 :: [String] -> String
part1 inputLines =
  inputLines
    & parseInput
    & getDiffs
    & countPositive
    & show

part2 :: [String] -> String
part2 inputLines =
  inputLines
    & parseInput
    & slideWindow
    & getDiffs
    & countPositive
    & show
