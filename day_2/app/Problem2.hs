module Main where

import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map

gameResultsMap = Map.fromList 
  [("A X", 3), -- Rock Loose
  ("A Y", 4),  -- Rock Tie
  ("A Z", 8),  -- Rock Win
  ("B X", 1),  -- Paper Loose
  ("B Y", 5),  -- Paper Tie
  ("B Z", 9),  -- Paper Win
  ("C X", 2),  -- Scizzors Loss
  ("C Y", 6),  -- Scissors Tie
  ("C Z", 7)]  -- Scissors Win

getResult :: String -> Int
getResult round = gameResultsMap Map.! round :: Int

generateInput :: [Char] -> [String]
generateInput contents = init (splitOn "\n" contents)

main = do 
  contents <- readFile "./input.txt"
  let input = generateInput contents
  let result = sum (map getResult input)
  print result