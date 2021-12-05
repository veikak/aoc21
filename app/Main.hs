module Main where

import Data.Function ((&))
import qualified Day01
import qualified Day02
import System.Environment (getArgs)
import Text.Printf (printf)
import Text.Read (readMaybe)

usage = "usage: aoc <day> <part> [input-file]"

parseInt :: String -> Maybe Int
parseInt arg = readMaybe arg :: Maybe Int

parseArgs :: [String] -> (Maybe Int, Maybe Int, Maybe String)
parseArgs [dayArg, partArg] = (parseInt dayArg, parseInt partArg, Nothing)
parseArgs [dayArg, partArg, inputArg] = (parseInt dayArg, parseInt partArg, Just inputArg)
parseArgs _ = (Nothing, Nothing, Nothing)

executeDay :: Int -> Int -> [String] -> String
executeDay 1 1 inputLines = Day01.part1 inputLines
executeDay 1 2 inputLines = Day01.part2 inputLines
executeDay 2 1 inputLines = Day02.part1 inputLines
executeDay _ part _ = "No such part: " ++ show part

main :: IO ()
main = do
  args <- getArgs
  let (day, part, inputName) = case parseArgs args of
        (Just day, Just part, Nothing) -> (day, part, "input.txt")
        (Just day, Just part, Just input) -> (day, part, input)
        (_, _, _) -> error usage
  input <- readFile $ "app/day" ++ printf "%02d" day ++ "/" ++ inputName
  putStrLn $ executeDay day part $ lines input
