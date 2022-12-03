module Main where

import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map

gameResultsMap = Map.fromList 
  [("A X", 4), -- Rock Draw
  ("A Y", 8), -- Rock Win
  ("A Z", 3), -- Rock Loose
  ("B X", 1), -- Paper Loose
  ("B Y", 5), -- Paper Draw
  ("B Z", 9), -- Paper Win
  ("C X", 7), -- Scizzors Win
  ("C Y", 2), -- Scissors Loose
  ("C Z", 6)]  -- Scissors Draw

getResult :: String -> Int
getResult round = gameResultsMap Map.! round :: Int

generateInput :: [Char] -> [String]
generateInput contents = init (splitOn "\n" contents)

main = do 
  contents <- readFile "./input.txt"
  let input = generateInput contents
  let result = sum (map getResult input)
  print result