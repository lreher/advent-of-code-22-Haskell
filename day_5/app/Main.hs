module Main where

import Data.List.Split
import Control.Lens
import Data.Sequence (fromList, Seq, update)
import Data.Foldable (toList)

move :: [String] -> Int -> Int -> Int -> Bool -> [String]
move crates moves from to isPart1 = toList (update from retained (update to added (fromList crates)))
  where 
    crateTaken = (crates !! from)
    crateAdded = (crates !! to)
    dropped = if isPart1 
                then reverse (take moves crateTaken)
                else (take moves crateTaken)
    retained = drop moves crateTaken
    added = dropped ++ crateAdded

zipCrates :: [[String]] -> [String]
zipCrates list = foldr (\elem acc -> zipWith (++) elem acc) [" " | _ <- [1..length (head list)]] list

splitInputs :: String -> [String]
splitInputs contents = splitOn "\n\n" contents

main = do 
  contents <- readFile "./input.txt"
  let [crateInput, moveInput] = splitInputs contents

  -- Crate Parsing
  let crateLines = init (splitOn "\n" crateInput)
  let splitCrateStrings = map (\x -> [[a] | a <- x]) crateLines
  let rawCrates = zipCrates splitCrateStrings
  let crates =  filter (\x -> x /= "") (map (filter (\y -> (y /= '[') && (y /= ']') && (y /= ' '))) rawCrates)

  -- Move Parsing
  let moveLines = lines moveInput
  let moveParts = map (chunksOf 2) (map (splitOn " ") moveLines)
  let moveStrings = map (\x -> [concat (tail a) | a <- x]) moveParts
  let moves =  map (map read) moveStrings :: [[Int]]

  -- -- Move Logic
  let resultPart1 = foldl (\acc elem -> (move acc (elem !! 0) ((elem !! 1) - 1) ((elem !! 2) - 1) True) ) crates moves
  let resultPart2 = foldl (\acc elem -> (move acc (elem !! 0) ((elem !! 1) - 1) ((elem !! 2) - 1) False) ) crates moves

  print resultPart1 
  print resultPart2
