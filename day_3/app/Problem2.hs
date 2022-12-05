module Main where

import Data.List
import Data.List.Split
import Data.Char
import Data.Ord

getPriorities :: Char -> Int
getPriorities item = if isLower item
                      then (fromEnum item) - 96        -- 1 .. 27
                      else ((fromEnum item) - 65) + 27 -- 27 .. 57

getUniqueGroups :: [String] -> [String]
getUniqueGroups allGroupInstances = (map head . group . sort) allGroupInstances

findMostRepeated :: String -> [Char]
findMostRepeated "" = []
findMostRepeated string = maximumBy (comparing length) (group (sort string))

getIntersects :: (String, [String]) -> [String] 
getIntersects values = map ((fst values) `intersect`) (snd values)

findIntersects :: [(String, [String])] -> [[String]]
findIntersects values = map getIntersects values

generateInput :: [Char] -> [String]
generateInput contents = init (splitOn "\n" contents)

main = do 
  contents <- readFile "./input.txt"
  let input = generateInput contents

  let tuples = [(x, [y | y <- input, y /= x ] ) | x <- input]
  let myIntersects = findIntersects tuples
  let rawRepeatedGroup = map (map findMostRepeated) myIntersects
  let rawGroups = getUniqueGroups (concat rawRepeatedGroup)
  let groups = [head x | x <- rawGroups, (length x) == 3]
  let result = sum (map getPriorities groups)

  print result