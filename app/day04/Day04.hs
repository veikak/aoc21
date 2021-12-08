module Day04 where

import AocUtil (every)
import Data.Function ((&))
import Data.List

type Draw = [Int]

type Board = [[Int]]

type FlatMaybeBoard = [Maybe Int]

ySize = 5

xSize = ySize

parseDraw :: String -> [Int]
parseDraw "" = []
parseDraw input = read rawNum : parseDraw (drop 1 rest)
  where
    (rawNum, rest) = break (== ',') input

parseBoardLine :: String -> [Int]
parseBoardLine "" = []
parseBoardLine input = if not $ null rawNum then read rawNum : next else next
  where
    (rawNum, rest) = break (== ' ') input
    next = parseBoardLine (drop 1 rest)

parseBoard :: [String] -> Board
parseBoard = map parseBoardLine

parseBoards :: [String] -> [Board]
parseBoards [] = []
parseBoards inputLines = parseBoard boardLines : parseBoards (drop 1 rest)
  where
    (boardLines, rest) = break ((==) 0 . length) inputLines

parseInput :: [String] -> (Draw, [Board])
parseInput inputLines = (parseDraw $ head inputLines, parseBoards $ drop 2 inputLines)

flattenBoard :: Board -> FlatMaybeBoard
flattenBoard board = concat board & map Just

processCell :: Int -> Maybe Int -> Maybe Int
processCell draw (Just cell)
  | cell == draw = Nothing
  | otherwise = Just cell
processCell _ Nothing = Nothing

matchBoard :: Int -> FlatMaybeBoard -> FlatMaybeBoard
matchBoard num = map (processCell num)

cellMatched :: Maybe Int -> Bool
cellMatched cell = case cell of
  Just cell -> False
  Nothing -> True

sliceMatched :: [Maybe Int] -> Bool
sliceMatched = all cellMatched

hasHorizontalWin :: FlatMaybeBoard -> Bool
hasHorizontalWin [] = False
hasHorizontalWin board = sliceMatched row || hasHorizontalWin rest
  where
    (row, rest) = splitAt xSize board

hasVerticalWin :: FlatMaybeBoard -> Int -> Bool
hasVerticalWin board (-1) = False
hasVerticalWin board n = sliceMatched column || hasVerticalWin board (n - 1)
  where
    column = replicate n (Just 0) ++ board & every xSize

boardHasWon :: FlatMaybeBoard -> Bool
boardHasWon board = hasHorizontalWin board || hasVerticalWin board (xSize - 1)

matchBoards :: Draw -> [FlatMaybeBoard] -> (FlatMaybeBoard, Int)
matchBoards [] boards = error "No match"
matchBoards (draw : restOfDraws) boards = case find boardHasWon processedBoards of
  Just board -> (board, draw)
  Nothing -> matchBoards restOfDraws processedBoards
  where
    processedBoards = map (matchBoard draw) boards

sumUnmarked :: FlatMaybeBoard -> Int
sumUnmarked board = [cell | Just cell <- board] & sum

part1 :: [String] -> String
part1 inputLines =
  [sumUnmarked winningBoard, finalDraw]
    & product
    & show
  where
    (draw, boards) =
      inputLines
        & parseInput
    (winningBoard, finalDraw) = matchBoards draw (map flattenBoard boards)
