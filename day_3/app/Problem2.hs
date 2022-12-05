module Main where

import Data.List
import Data.List.Split
import Data.Char

getPriorities :: [Char] -> Int
getPriorities item = if isLower (head item)
                      then (fromEnum (head item)) - 96        -- 1 .. 27
                      else ((fromEnum (head item)) - 65) + 27 -- 27 .. 57

getIntersects :: [String] -> [Char]
getIntersects [x, y ,z] = x `intersect` y `intersect` z

generateInput :: [Char] -> [String]
generateInput contents = init (splitOn "\n" contents)

main = do 
  contents <- readFile "./input.txt"
  let input = generateInput contents
  let groups = chunksOf 3 input
  let list = map getIntersects groups
  let result = sum (map getPriorities list)

  print result