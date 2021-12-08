module AocUtil where

-- https://stackoverflow.com/a/2028218
every n xs = case drop (n -1) xs of
  y : ys -> y : every n ys
  [] -> []
