module Main where

import Data.Char (digitToInt)
import Data.Maybe (fromMaybe)
import Data.List (find, sort, group, permutations)
import qualified Data.Map as M
import qualified Data.Vector as V ((!?), fromList, ifoldl, length)

main :: IO ()
main = do
  putStrLn $ show $ nearestSpiralVal 325489

--
-- Day 4
--

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

--
-- Day 3
--

getCircle :: Int -> Int
getCircle index = (if num `mod` 2 == 0 then num - 1 else num) `div` 2
  where num = ceiling $ sqrt $ fromIntegral index

manhattanDistance :: Int -> Int
manhattanDistance i = circle + (abs $ (i - 1) `mod` (2 * circle) - circle)
  where circle = getCircle i

-- Part 2

data Dir = Left | Down | Right | Up

type Point = (Int, Int)

data Spiral = Spiral
  { getDir :: Dir
  , getPoint :: Point
  , getPMap :: M.Map Point Int
  }

newSpiral :: Spiral
newSpiral = Spiral Up (1, 0) $ M.fromList [((0, 0), 1), ((1, 0), 1)]

nearestSpiralVal :: Int -> Int
nearestSpiralVal val = getVal newSpiral
  where getVal spiral = if spiralVal spiral > val
                           then spiralVal spiral
                           else getVal $ nextSpiral spiral

spiralVal :: Spiral -> Int
spiralVal (Spiral _ point pMap) = fromMaybe 0 $ M.lookup point pMap

nextSpiral :: Spiral -> Spiral
nextSpiral spiral = Spiral newDir newPoint $ M.insert newPoint (dirSum pMap newPoint) pMap
  where
    newDir = if canTurn spiral then succDir dir else dir
    newPoint = nextPoint newDir $ getPoint spiral
    pMap = getPMap spiral
    dir = getDir spiral

dirSum :: M.Map Point Int -> Point -> Int
dirSum pMap point = sum $ map getValue points
  where
    getValue key = fromMaybe 0 $ M.lookup (addPoint key point) pMap
    points = [(1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1)]

succDir :: Dir -> Dir
succDir Main.Left = Down
succDir Down = Main.Right
succDir Main.Right = Up
succDir Up = Main.Left

canTurn :: Spiral -> Bool
canTurn (Spiral dir point pMap) = not $ M.member key pMap
  where key = nextPoint (succDir dir) point

addPoint :: Point -> Point -> Point
addPoint (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

nextPoint :: Dir -> Point -> Point
nextPoint dir p1 = addPoint p1 p2
  where p2 = case dir of
               Up -> (0, 1)
               Down -> (0, -1)
               Main.Left -> (-1, 0)
               Main.Right -> (1, 0)

--
-- Day 2
--

checksum :: [[Int]] -> Int
checksum = sum . map rowChecksum
  where rowChecksum row = (maximum row) - (minimum row)

divChecksum :: [[Int]] -> Int
divChecksum = sum . map rowChecksum
  where
    rowChecksum row = fromMaybe 0
      $ fmap (uncurry div)
      $ find (\(x, y) -> x /= y && x `mod` y == 0)
      $ concat
      $ map (zip row . repeat) row

sampleSheet :: [[Int]]
sampleSheet =
  [ [5, 9, 2, 8]
  , [9, 4, 7, 3]
  , [3, 8, 6, 5]
  ]

--
-- Day 1
--

captchaSum :: String -> Int
captchaSum input = snd $ foldl helper (Nothing, 0) input
  where
    helper (prev, total) char =
      let next = digitToInt char
      in (Just next, total + (getNext prev next))
    getNext maybePrev next =
      let prev = fromMaybe (digitToInt $ last input) maybePrev
      in if prev == next then next else 0

captchaSumCircular :: String -> Int
captchaSumCircular input = V.ifoldl helper 0 vec
  where
    vec = V.fromList input
    inputLength = V.length vec
    getNext (Just x) next = if next == x then x else 0
    getNext Nothing _ = 0
    helper total i next =
      let half = vec V.!? (mod (i + inputLength `div` 2) inputLength)
      in (+) total $ getNext (digitToInt <$> half) (digitToInt next)
