module Day7 where

import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Text.ParserCombinators.ReadP as P

solution :: IO (String, String)
solution = do
  input <- getTowers
  return (findRoot input , "0")

findRoot :: [Tower] -> String
findRoot input = fromMaybe "" $ fmap getName $ find (not . (flip S.member) set . getName) input
  where set = S.fromList $ concat $ map getLeaves input

data Tower = Tower
  { getName :: String
  , getWeight :: Int
  , getLeaves :: [String]
  }

-- File Parser

getTowers :: IO [Tower]
getTowers = do
  input <- readFile "src/day7_towers.txt"
  return $ fst $ head $ P.readP_to_S towers input

towers :: ReadP [Tower]
towers = many1 tower <* P.eof

tower :: ReadP Tower
tower =
  Tower
    <$> name
    <* P.skipSpaces
    <*> P.between (P.char '(') (P.char ')') number
    <*> P.option [] leaves
    <* P.char '\n'

leaves :: ReadP [String]
leaves = P.skipSpaces
  <* P.string "->"
  <* P.skipSpaces
  *> P.sepBy1 name (P.string ", ")

name :: ReadP String
name = P.many1 $ P.satisfy (\char -> char >= 'a' && char <= 'z')

number :: ReadP Int
number = read <$> (P.many1 $ P.satisfy (\char -> char >= '0' && char <= '9'))
