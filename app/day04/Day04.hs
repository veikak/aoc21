module Day04 where

import AocUtil
import Data.Function ((&))
import Data.List
import Debug.Trace

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

matchBoards :: Bool -> Draw -> [FlatMaybeBoard] -> Maybe (FlatMaybeBoard, Int) -> (FlatMaybeBoard, Int)
matchBoards True [] _ lastWin = case lastWin of
  Just win -> win
  Nothing -> error "No wins"
matchBoards _ [] _ _ = error "No match"
matchBoards pickLastWinner (draw : restOfDraws) boards lastWins = case findIndices boardHasWon processedBoards of
  [] -> matchBoards pickLastWinner restOfDraws processedBoards lastWins
  indices ->
    if pickLastWinner
      then matchBoards pickLastWinner restOfDraws (dropIndices indices processedBoards) (Just (processedBoards !! last indices, draw))
      else (processedBoards !! head indices, draw)
  where
    processedBoards = map (matchBoard draw) boards

sumUnmarked :: FlatMaybeBoard -> Int
sumUnmarked board = [cell | Just cell <- board] & sum

part1 :: [String] -> String
part1 inputLines =
  [sumUnmarked winningBoard, winningDraw]
    & product
    & show
  where
    (draw, boards) =
      inputLines
        & parseInput
    (winningBoard, winningDraw) = matchBoards False draw (map flattenBoard boards) Nothing

part2 :: [String] -> String
part2 inputLines =
  [sumUnmarked winningBoard, winningDraw]
    & product
    & show
  where
    (draw, boards) =
      inputLines
        & parseInput
    (winningBoard, winningDraw) = matchBoards True draw (map flattenBoard boards) Nothing
