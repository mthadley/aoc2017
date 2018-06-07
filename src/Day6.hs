module Day6 where

import           Data.Maybe    (fromMaybe)
import           Data.Sequence (Seq, adjust, elemIndexL, fromList, index,
                                update)
import           Data.Set      (insert, member, singleton)

solution :: IO (String, String)
solution = do
  return
    (show $ distToFirstCycle problemInput
    , show $ findCycleLength problemInput
    )

problemInput :: Seq Int
problemInput = fromList [5, 1, 10, 0, 1, 7, 13, 14, 3, 12, 8, 10, 7, 12, 0, 6]

distToFirstCycle :: Seq Int -> Int
distToFirstCycle = fst . findRepeat

findCycleLength :: Seq Int -> Int
findCycleLength input = countDist 1 repeatedBank
  where
    repeatedBank = snd $ findRepeat input
    countDist count banks
      | newBanks == repeatedBank = count
      | otherwise = countDist (count + 1) newBanks
        where newBanks = redistribute banks

findRepeat :: Seq Int -> (Int, Seq Int)
findRepeat input = countDist (singleton input) input
  where
    countDist set banks
      | member newBanks set = (length set, newBanks)
      | otherwise = countDist (insert newBanks set) newBanks
        where newBanks = redistribute banks

redistribute :: Seq Int -> Seq Int
redistribute banks = applyToBanks (maxIndex + 1) (index banks maxIndex) newBanks
  where
    maxIndex = fromMaybe 0 $ (flip elemIndexL) banks $ maximum banks
    newBanks = update maxIndex 0 banks

applyToBanks :: Int -> Int -> Seq Int -> Seq Int
applyToBanks _ 0 banks = banks
applyToBanks startIndex value banks =
  applyToBanks (i + 1) (value - 1)
    $ adjust ((+) 1) i banks
      where i = startIndex `mod` length banks
