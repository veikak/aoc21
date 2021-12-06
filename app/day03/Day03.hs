module Day03 where

import Data.Function ((&))
import Data.List ( foldl' )
import Data.Char (digitToInt)

countOnes :: [String] -> [Int]
countOnes binStrs = foldl (zipWith (\accElem binStrElem -> accElem + digitToInt binStrElem)) initialValue binStrs
  where initialValue = replicate (length (head binStrs)) 0

mostCommonBits :: Int -> [Int] -> [Int]
mostCommonBits totalCount = map (fromEnum . (totalCount `div` 2 <))

invertBits :: [Int] -> [Int]
invertBits = map (1 -)

applyBit :: Int -> (Int, Int) -> Int
applyBit acc (bit, bitNum) = acc + bit * (2 ^ bitNum)

binIntListToNum :: [Int] -> Int
binIntListToNum bin = foldl' applyBit 0 input
  where input = zip bin $ reverse [0 .. length bin - 1]

part1 :: [String] -> String
part1 inputLines = [commonBits, invertBits commonBits]
  & map binIntListToNum
  & product
  & show
  where
    numLines = length inputLines
    commonBits = inputLines
      & countOnes
      & mostCommonBits numLines
