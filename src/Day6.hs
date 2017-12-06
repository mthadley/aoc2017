module Day6 where

import Data.Set (insert, member, singleton)
import qualified Data.Vector.Mutable as M (modify, write)
import Data.Maybe (fromMaybe)
import Data.Vector (Vector, elemIndex, fromList, modify, (!))

solution :: IO (Int, Int)
solution = do
  return (distToFirstCycle input, findCycleLength input)

input :: Vector Int
input = fromList [5, 1, 10, 0, 1, 7, 13, 14, 3, 12, 8, 10, 7, 12, 0, 6]

distToFirstCycle :: Vector Int -> Int
distToFirstCycle = fst . findRepeat

findCycleLength :: Vector Int -> Int
findCycleLength input = countDist 1 repeatedBank
  where
    repeatedBank = snd $ findRepeat input
    countDist count banks
      | newBanks == repeatedBank = count
      | otherwise = countDist (count + 1) newBanks
        where newBanks = redistribute banks

findRepeat :: Vector Int -> (Int, Vector Int)
findRepeat = countDist $ singleton input
  where
    countDist set banks
      | member newBanks set = (length set, newBanks)
      | otherwise = countDist (insert newBanks set) newBanks
        where newBanks = redistribute banks

redistribute :: Vector Int -> Vector Int
redistribute banks = applyToBanks (maxIndex + 1) (banks ! maxIndex) newBanks
  where
    maxIndex = fromMaybe 0 $ (flip elemIndex) banks $ maximum banks
    newBanks = modify (\v -> M.write v maxIndex 0) banks

applyToBanks :: Int -> Int -> Vector Int -> Vector Int
applyToBanks _ 0 banks = banks
applyToBanks startIndex value banks =
  applyToBanks (index + 1) (value - 1)
    $ modify (\v -> M.modify v ((+) 1) index) banks
      where index = startIndex `mod` length banks
