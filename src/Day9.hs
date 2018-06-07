module Day9 where

solution :: IO (String, String)
solution = do
  input <- readFile "src/day9_groups.txt"
  return
    ( show $ getScore input
    , show $ getGarbage input
    )

getScore :: String -> Int
getScore input = let (total, _, _ ,_) = foldl score (0, 0, False, False) input
                  in total

score :: (Int, Int, Bool, Bool) -> Char -> (Int, Int, Bool, Bool)
score (total, level, inGarbage, True) _ = (total, level, inGarbage, False)
score (total, level, inGarbage, _) '!' = (total, level, inGarbage, True)
score (total, level, _, inEscape) '>' = (total, level, False, inEscape)
score (total, level, _, inEscape) '<' = (total, level, True, inEscape)
score (total, level, True, inEscape) _ = (total, level, True, inEscape)
score (total, level, inGarbage, inEscape) '{' = (total, level + 1, inGarbage, inEscape)
score (total, level, inGarbage, inEscape) '}' = (total + level, level - 1, inGarbage, inEscape)
score (total, level, inGarbage, inEscape) _ = (total, level, inGarbage, inEscape)

-- 5404 too high
getGarbage :: String -> Int
getGarbage input = let (total, _, _) = foldl garbage (0, False, False) input
                  in total

garbage :: (Int, Bool, Bool) -> Char -> (Int, Bool, Bool)
garbage (total, inGarbage, True) _     = (total, inGarbage, False)
garbage (total, inGarbage, _) '!'      = (total, inGarbage, True)
garbage (total, True, inEscape) '>'    = (total, False, inEscape)
garbage (total, False, inEscape) '<'   = (total, True, inEscape)
garbage (total, True, inEscape) _      = (total + 1, True, inEscape)
garbage (total, inGarbage, inEscape) _ = (total, inGarbage, inEscape)
