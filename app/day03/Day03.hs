module Day03 where

import Data.Function ((&))
import Data.List ( foldl' )
import Data.Char (digitToInt)

sumBits :: [String] -> [Int]
sumBits binStrs = foldl (zipWith (\accElem binStrElem -> accElem + digitToInt binStrElem)) initialValue binStrs
  where initialValue = replicate (length (head binStrs)) 0

mostCommonBits :: Int -> [Int] -> [Int]
mostCommonBits totalCount = map (fromEnum . (\bitSum -> totalCount - bitSum <= bitSum))

binStrsToMostCommonBits :: [String] -> [Int]
binStrsToMostCommonBits inputLines = inputLines & sumBits & mostCommonBits (length inputLines)

invertBits :: [Int] -> [Int]
invertBits = map (1 -)

applyBit :: Int -> (Int, Int) -> Int
applyBit acc (bit, bitNum) = acc + bit * (2 ^ bitNum)

binIntListToNum :: [Int] -> Int
binIntListToNum bin = foldl' applyBit 0 input
  where input = zip bin $ reverse [0 .. length bin - 1]

filterByBit :: [String] -> Int -> ([String] -> [Int]) -> String
filterByBit [] _ _ = error "No match"
filterByBit [inputLine] _ _ = inputLine
filterByBit inputLines bitNum matcher = filterByBit filteredLines (bitNum + 1) matcher
  where
    matchedBits = matcher inputLines
    filteredLines = filter (\line -> digitToInt (line !! bitNum) == (matchedBits !! bitNum)) inputLines

applyStrBit :: Int -> (Char, Int) -> Int
applyStrBit acc (bit, bitNum) = acc + digitToInt bit * (2 ^ bitNum)

binStrToNum :: String -> Int
binStrToNum bin = foldl' applyStrBit 0 input
  where input = zip bin $ reverse [0 .. length bin - 1]

part1 :: [String] -> String
part1 inputLines = [commonBits, invertBits commonBits]
  & map binIntListToNum
  & product
  & show
  where
    numLines = length inputLines
    commonBits = inputLines
      & sumBits
      & mostCommonBits numLines

part2 :: [String] -> String
part2 inputLines = [binStrsToMostCommonBits, invertBits . binStrsToMostCommonBits]
  & map (filterByBit inputLines 0)
  & map binStrToNum
  & product
  & show
  where
    numLines = length inputLines
