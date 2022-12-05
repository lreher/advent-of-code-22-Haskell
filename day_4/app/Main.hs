module Main where

import Data.List.Split

checkPartialOverlap :: [(Int, Int)] -> Bool
checkPartialOverlap [(x1, y1), (x2, y2)] = (x1 <= y2 && y1 >= x2) || (x2 <= y1 && y2 >= x1)

checkOverlap :: [(Int, Int)] -> Bool
checkOverlap [(x1, y1), (x2, y2)] = (x1 <= x2 && y1 >= y2) || (x2 <= x1 && y2 >= y1)

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x, y)

convertToInt :: [String] -> [Int]
convertToInt stringList = map read stringList :: [Int]

makeTuples :: String -> (Int, Int)
makeTuples input = tuplify2 (convertToInt (splitOn "-" input))

generateInput :: String -> [[String]]
generateInput contents =  map (splitOn ",") (lines contents)

main = do 
  contents <- readFile "./input.txt"
  let input = generateInput contents
  let tuples = map (map makeTuples) input
  let result1 = length (filter (\n -> n == True) (map checkOverlap tuples))
  let result2 = length (filter (\n -> n == True) (map checkPartialOverlap tuples))

  print result1
  print result2
