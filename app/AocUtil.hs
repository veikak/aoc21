module AocUtil where

import Data.Function ((&))

-- https://stackoverflow.com/a/2028218
every n xs = case drop (n -1) xs of
  y : ys -> y : every n ys
  [] -> []

dropNth :: Int -> [a] -> [a]
dropNth n xs = take n xs ++ drop (n + 1) xs

dropIndices :: [Int] -> [a] -> [a]
dropIndices is xs = zip xs [0 .. (length xs - 1)] & filter (\(a, b) -> b `notElem` is) & map fst
