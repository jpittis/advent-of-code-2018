#!/usr/bin/env stack
-- stack --resolver lts-12.20 --install-ghc runghc --package containers

module Main where

import qualified Data.Set as Set

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let nums = map (readInt . removePlus) input
  print $ sum nums
  let inf = cycle nums
  print $ sumAndLookForDoubles 0 Set.empty inf
  where
    removePlus = filter (not . (==) '+')
    readInt = (read :: String -> Int)
    sumAndLookForDoubles total set (x:xs) =
      let nextTotal = total + x in
      if Set.member nextTotal set then nextTotal else
        sumAndLookForDoubles nextTotal (Set.insert nextTotal set) xs
