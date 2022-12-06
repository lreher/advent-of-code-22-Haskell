module Main where

import Data.List
import Data.List.Split
import Data.Ord
import Data.Containers.ListUtils

signalLookup :: String -> Int -> String
signalLookup string characters = if (length (nubOrd (take characters string))) /= characters
                 then signalLookup (tail string) characters
                 else take characters string

main = do 
  contents <- readFile "./input.txt"
  let input = contents

  -- answer 1
  let signal1 = signalLookup input 4
  let result1 = length (head (splitOn signal1 input)) + 4

   -- answer 2
  let signal2 = signalLookup input 14
  let result2 = length (head (splitOn signal2 input)) + 14

  print result1
  print result2