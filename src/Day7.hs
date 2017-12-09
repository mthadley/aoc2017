module Day7 where

import Control.Applicative ((<|>))
import Control.Monad (msum)
import Control.Monad.Reader (Reader, runReader, ask)
import Data.Function (on)
import Data.List (find, groupBy, sortBy)
import Data.Maybe (fromMaybe, mapMaybe, isJust)
import qualified Data.Map as M
import qualified Data.Set as S
import Text.ParserCombinators.ReadP as P

solution :: IO (String, String)
solution = do
  input <- readTowers
  return
    ( findRootName input
    , "23"
    )

-- Part 1

findRootName :: [Tower] -> String
findRootName = fromMaybe "Not Found" . fmap getName . findRoot

findRoot :: [Tower] -> Maybe Tower
findRoot input = find (not . (flip S.member) set . getName) input
  where set = S.fromList $ concat $ map getLeaves input

-- Part 2

findCorrectWeight towers = ((runReader findUnbalancedTower) towerMap) =<< findRoot towers
  where
    towerMap = M.fromList $ map (\t -> (getName t, t)) towers
    findUnbalancedTower tower = do
      targetWeight <- getTargetWeight tower
      leaves <- getTowers $ getLeaves tower
      towerWeights <- mapM towerWeight leaves
      (return $ msum $ map findUnbalancedTower leaves) <|> ((return $ correctWeight leaves) <$ (find (((/=) targetWeight)) towerWeights))

correctWeight :: [Tower] -> TowerP Int
correctWeight towers = do
  towerWeights <- mapM towerWeight towers
  let [[(_, bad)], (_, good):_] = sortBy (compare `on` length) $ groupBy (\(a, _) (b, _) -> a == b) $ zip towerWeights towers
  goodWeight <- towerWeight  good
  badLeavesWeights <- mapM towerWeight =<< (getTowers $ getLeaves bad)
  return $ goodWeight - (sum badLeavesWeights)

getTargetWeight :: Tower -> TowerP Int
getTargetWeight tower@(Tower _ weight leaves) = do
  totalWeight <- towerWeight tower
  return $ (totalWeight - weight) `div` (length leaves)

towerWeight :: Tower -> TowerP Int
towerWeight (Tower _ weight leaves) = do
  leavesWeights <- mapM towerWeight =<< getTowers leaves
  return $ weight + (sum leavesWeights)

getTowers :: [String] -> TowerP [Tower]
getTowers towers = do
  towerMap <- ask
  return $ mapMaybe ((flip M.lookup) towerMap) towers

type TowerP a = Reader (M.Map String Tower) a

data Tower = Tower
  { getName :: String
  , getWeight :: Int
  , getLeaves :: [String]
  }

-- File Parser

readTowers :: IO [Tower]
readTowers = do
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
