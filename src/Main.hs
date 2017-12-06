module Main where

import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import Text.Read (readMaybe)

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6

solutions :: [IO (Int, Int)]
solutions =
  [ Day1.solution
  , Day2.solution
  , Day3.solution
  , Day4.solution
  , Day5.solution
  , Day6.solution
  ]

main :: IO ()
main = do
  day <- getDay $ length solutions
  printParts =<< solutions !! (day - 1)

printParts :: (Show a, Show b) => (a, b) -> IO ()
printParts (part1, part2) = do
  putStrLn $ "Part 1: " ++ (show part1)
  putStrLn $ "Part 2: " ++ (show part2)

getDay :: Int -> IO Int
getDay defaultDay = do
  args <- getArgs
  let day = case args of
           ["-d", num] -> fromMaybe defaultDay $ readMaybe num
           otherwise -> defaultDay
  return $ min defaultDay $ max 1 day
