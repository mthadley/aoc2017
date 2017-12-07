module Day5 where

import qualified Data.Vector.Mutable as M (modify)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.Vector (Vector, fromList, (!?), modify)

solution :: IO (String, String)
solution = do
  jumps <- getInputJumps
  return (show $ jumpCountAdd jumps, show $ jumpCountAddThree jumps)

data CPU = CPU
  { getJumps :: Vector Int
  , getIndex :: Int
  }

initCPU :: Vector Int -> CPU
initCPU jumps = CPU jumps 0

jumpCountAdd :: Vector Int -> Int
jumpCountAdd = jumpCountWith $ (+) 1

jumpCountAddThree :: Vector Int -> Int
jumpCountAddThree = jumpCountWith addThree

jumpCountWith :: (Int -> Int) -> Vector Int -> Int
jumpCountWith f = runToCompletion 0 . initCPU
  where
    runToCompletion :: Int -> CPU -> Int
    runToCompletion count cpu = fromMaybe count $ do
      newCPU <- nextState f cpu
      return $ runToCompletion (count + 1) newCPU

nextState :: (Int -> Int) -> CPU -> Maybe CPU
nextState f (CPU jumps index) = do
  jump <- jumps !? index
  return $ CPU (updateAt f index jumps) $ index + jump

addThree :: Int -> Int
addThree jump = if jump >= 3
                 then jump - 1
                 else jump + 1

updateAt :: (Int -> Int) -> Int -> Vector Int -> Vector Int
updateAt f index = modify (\v -> M.modify v f index )

getInputJumps :: IO (Vector Int)
getInputJumps = do
  jumps <- lines <$> readFile "src/day5_jumps.txt"
  return $ fromList $ fromMaybe 0 <$> readMaybe <$> jumps
