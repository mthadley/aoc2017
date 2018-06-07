module Day4 where

import           Data.List (group, sort)

solution :: IO (String, String)
solution = do
  passwords <- readPasswords
  return
    ( show $ countWith hasDuplicateWords passwords
    , show $ countWith noAnagrams passwords
    )

countWith :: (String -> Bool) -> [String] -> Int
countWith f = length . filter f

readPasswords :: IO [String]
readPasswords = lines <$> readFile "src/day4_passwords.txt"

hasDuplicateWords :: String -> Bool
hasDuplicateWords = all ((==) 1 . length) . group . sort . words

noAnagrams :: String -> Bool
noAnagrams input = not $ any otherAnagrams $ zip [1..] pws
  where
    pws = words input
    otherAnagrams (i, word) = any (isAnagram word) $ drop i pws

isAnagram :: String -> String -> Bool
isAnagram a b = sort a == sort b
