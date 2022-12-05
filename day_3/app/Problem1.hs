module Main where

import Data.List
import Data.List.Split
import Data.Char

-- We can assume repeat items will always be the same 
-- Use ASCI to calculate priority 
getPriorities :: [Char] -> Int
getPriorities item = if isLower (head item)
                      then (fromEnum (head item)) - 96        -- 1 .. 27
                      else ((fromEnum (head item)) - 65) + 27 -- 27 .. 57

getIntersects :: [String] -> [Char]
getIntersects rucksack = (head rucksack) `intersect` (last rucksack)

splitRucksack :: String -> [String]
splitRucksack rucksack = [take ((length rucksack) `div` 2) rucksack, drop ((length rucksack) `div` 2) rucksack]

generateInput :: [Char] -> [[String]]
generateInput contents = map splitRucksack (init (splitOn "\n" contents))

main = do 
  contents <- readFile "./input.txt"
  let input = generateInput contents
  let intersects = map getIntersects input
  let total = sum (map getPriorities intersects)

  print total