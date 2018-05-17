module Day3 where

import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

solution :: IO (String,  String)
solution = return
  ( show $ manhattanDistance input
  , show $ nearestSpiralVal input
  )

input :: Int
input = 325489

getCircle :: Int -> Int
getCircle index = (if num `mod` 2 == 0 then num - 1 else num) `div` 2
  where num = (ceiling :: Double -> Int) $ sqrt $ fromIntegral index

manhattanDistance :: Int -> Int
manhattanDistance i = circle + (abs $ (i - 1) `mod` (2 * circle) - circle)
  where circle = getCircle i

-- Part 2

data Dir = Left | Down | Right | Up

type Point = (Int, Int)

data Spiral = Spiral
  { getDir :: Dir
  , getPoint :: Point
  , getPMap :: Map.Map Point Int
  }

newSpiral :: Spiral
newSpiral = Spiral Up (1, 0) $ Map.fromList [((0, 0), 1), ((1, 0), 1)]

nearestSpiralVal :: Int -> Int
nearestSpiralVal val = getVal newSpiral
  where getVal spiral = if spiralVal spiral > val
                           then spiralVal spiral
                           else getVal $ nextSpiral spiral

spiralVal :: Spiral -> Int
spiralVal (Spiral _ point pMap) = fromMaybe 0 $ Map.lookup point pMap

nextSpiral :: Spiral -> Spiral
nextSpiral spiral = Spiral newDir newPoint $ Map.insert newPoint (dirSum pMap newPoint) pMap
  where
    newDir = if canTurn spiral then succDir dir else dir
    newPoint = nextPoint newDir $ getPoint spiral
    pMap = getPMap spiral
    dir = getDir spiral

dirSum :: Map.Map Point Int -> Point -> Int
dirSum pMap point = sum $ map getValue points
  where
    getValue key = fromMaybe 0 $ Map.lookup (addPoint key point) pMap
    points = [(1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1)]

succDir :: Dir -> Dir
succDir Day3.Left = Down
succDir Down = Day3.Right
succDir Day3.Right = Up
succDir Up = Day3.Left

canTurn :: Spiral -> Bool
canTurn (Spiral dir point pMap) = not $ Map.member key pMap
  where key = nextPoint (succDir dir) point

addPoint :: Point -> Point -> Point
addPoint (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

nextPoint :: Dir -> Point -> Point
nextPoint dir p1 = addPoint p1 p2
  where p2 = case dir of
               Up -> (0, 1)
               Down -> (0, -1)
               Day3.Left -> (-1, 0)
               Day3.Right -> (1, 0)
