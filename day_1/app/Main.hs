module Main where

import Data.List.Split
import Data.List
import Data.Ord

convertInt :: [String] -> [Int]
convertInt list = map read list :: [Int]

generateInput :: [Char] -> [[Int]]
generateInput contents = map convertInt (map lines (splitOn "\n\n" contents))

main = do 
  contents <- readFile "./day1_input.txt"
  let input = generateInput contents

  -- Part 1
  -- let fattytElf = maximum (map sum input)

  -- Part 2
  -- let fattytElf = sum (take 3 (sortOn Down (map sum input)))

  print fattytElf