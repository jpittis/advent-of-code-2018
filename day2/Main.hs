#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc --package containers

module Main where

import qualified Data.Map.Strict as Map

input = lines <$> readFile "input.txt"

main :: IO ()
main = do
  input <- input
  let solution1 = puzzle1 input
  let solution2 = puzzle2 input
  print solution1
  print solution2

puzzle1 input =
  let two = filter (> 0) . map (\str -> count str 2) $ input in
  let three = filter (> 0) . map (\str -> count str 3) $ input in
  checksum (length two) (length three)

count str num =
  found num $ Map.elems counts
  where
    counts = foldl (\m c -> Map.insertWith (+) c 1 m) Map.empty str
    found x xs = length $ filter (\v -> x == v) xs

checksum two three = two * three

puzzle2 input =
  let product = sequence [input, input] in
  let ids = filter (\li -> length li == 25) $ map diff product in
  map (\(a, b) -> a) (head ids)

diff [a, b] = filter (\(x, y) -> x == y) $ zip a b
