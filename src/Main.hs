module Main where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4

solutions :: [IO (Int, Int)]
solutions =
  [ Day1.solution
  , Day2.solution
  , Day3.solution
  , Day4.solution
  ]

main :: IO ()
main = printSolutions =<< last solutions

printSolutions :: (Show a, Show b) => (a, b) -> IO ()
printSolutions (part1, part2) = do
  putStr "Part 1: "
  putStrLn $ show $ part1
  putStr "Part 2: "
  putStrLn $ show $ part2
