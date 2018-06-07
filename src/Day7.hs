module Day7 where

import           Control.Applicative          ((<|>))
import           Control.Monad                (msum)
import           Data.Function                (on)
import           Data.List                    (find, groupBy, sortBy)
import qualified Data.Map                     as M
import           Data.Maybe                   (fromMaybe, mapMaybe)
import qualified Data.Set                     as S
import           Text.ParserCombinators.ReadP as P

solution :: IO (String, String)
solution = do
  input <- readTowers
  return
    ( findRootName input
    , show $ findCorrectWeight input
    )

-- Part 1

findRootName :: [Tower] -> String
findRootName = fromMaybe "Not Found" . fmap getName . findRoot

findRoot :: [Tower] -> Maybe Tower
findRoot input = find (not . (flip S.member) set . getName) input
  where set = S.fromList $ concat $ map getLeaves input

-- Part 2

findCorrectWeight :: [Tower] -> Maybe Int
findCorrectWeight towers = findUnbalancedTower =<< findRoot towers
  where
    towerMap = M.fromList $ map (\t -> (getName t, t)) towers
    findUnbalancedTower tower =
      (msum $ map findUnbalancedTower leaves)
      <|>
      correctWeight towerMap leaves <$ (find (((/=) targetWeight) . towerWeight towerMap) leaves)
        where
          targetWeight = getTargetWeight towerMap tower
          leaves = getTowers towerMap $ getLeaves tower

correctWeight :: M.Map String Tower -> [Tower] -> Int
correctWeight towerMap towers = towerWeight towerMap good - badLeavesWeight
  where
    [[bad], good:_] = sortBy (compare `on` length)
      $ groupBy (\a b -> towerWeight towerMap a == towerWeight towerMap b) towers
    badLeavesWeight = sum $ map (towerWeight towerMap) $ getTowers towerMap $ getLeaves bad

getTargetWeight :: M.Map String Tower -> Tower -> Int
getTargetWeight towerMap tower@(Tower _ weight leaves) =
  (towerWeight towerMap tower - weight) `div` (length leaves)

towerWeight :: M.Map String Tower -> Tower -> Int
towerWeight towerMap (Tower _ weight leaves) = weight + leavesWeight
  where
    leavesWeight = sum $ map (towerWeight towerMap) $ getTowers towerMap leaves

getTowers :: M.Map String Tower -> [String] -> [Tower]
getTowers towerMap = mapMaybe ((flip M.lookup) towerMap)

data Tower = Tower
  { getName   :: String
  , getWeight :: Int
  , getLeaves :: [String]
  }

-- File Parser

readTowers :: IO [Tower]
readTowers = do
  input <- readFile "src/day7_towers.txt"
  return $ fst $ head $ P.readP_to_S parseTowers input

parseTowers :: ReadP [Tower]
parseTowers = many1 parseTower <* P.eof

parseTower :: ReadP Tower
parseTower =
  Tower
    <$> name
    <* P.skipSpaces
    <*> P.between (P.char '(') (P.char ')') number
    <*> P.option [] parseLeaves
    <* P.char '\n'

parseLeaves :: ReadP [String]
parseLeaves = P.skipSpaces
  <* P.string "->"
  <* P.skipSpaces
  *> P.sepBy1 name (P.string ", ")

name :: ReadP String
name = P.many1 $ P.satisfy (\c -> c >= 'a' && c <= 'z')

number :: ReadP Int
number = read <$> (P.many1 $ P.satisfy (\c -> c >= '0' && c <= '9'))
