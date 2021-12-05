module Main where

import Data.Function ((&))
import Day01 (day01)
import GHC.IO
import System.Environment (getArgs)
import Text.Printf (printf)
import Text.Read (readMaybe)

usage = "usage: aoc <day> [input-file]"

parseDay :: String -> Maybe Int
parseDay arg = readMaybe arg :: Maybe Int

parseArgs :: [String] -> (Maybe Int, Maybe String)
parseArgs [dayArg] = (parseDay dayArg, Nothing)
parseArgs [dayArg, inputArg] = (parseDay dayArg, Just inputArg)
parseArgs _ = (Nothing, Nothing)

executeDay :: Int -> [String] -> String
executeDay day inputLines
  | day == 1 = day01 inputLines
  | otherwise = ""

main :: IO ()
main = do
  args <- getArgs
  let parsedArgs = parseArgs args
  let (day, inputName) = case parsedArgs of
        (Just day, Nothing) -> (day, "input.txt")
        (Just day, Just input) -> (day, input)
        (_, _) -> error usage
  input <- "app/day" ++ printf "%02d" day ++ "/" ++ inputName & readFile
  putStrLn $ executeDay day $ lines input
