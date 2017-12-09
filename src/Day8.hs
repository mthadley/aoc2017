module Day8 where

import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))
import qualified Data.Map as M
import Text.ParserCombinators.ReadP as P

solution :: IO (String, String)
solution = do
  input <- readInstructions
  return
    ( show $ getMaxRegister input
    , show $ getMaxInsertedValue input
    )

getMaxRegister :: [Instruction] -> Int
getMaxRegister = maximum . map snd . M.toList . foldl helper M.empty
  where helper regs ins = fst $ process regs ins

getMaxInsertedValue :: [Instruction] -> Int
getMaxInsertedValue = snd . foldl helper (M.empty, 0)
  where
    helper (regs, maxValue) i = mapSnd (max maxValue) $ process regs i

process :: M.Map String Int -> Instruction -> (M.Map String Int, Int)
process regs (Instruction target op amount condReg comp condAmount) =
  if not $ comp (M.findWithDefault 0 condReg regs) condAmount
     then (regs, 0)
     else let val = (flip op) amount $ M.findWithDefault 0 target regs
           in (M.insert target val regs, val)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

data Instruction = Instruction
  { getTarget :: String
  , getOp :: Int -> Int -> Int
  , getAmount :: Int
  , getCondReg :: String
  , getComp :: Int -> Int -> Bool
  , getCondAmount :: Int
  }

-- File Parser

readInstructions :: IO [Instruction]
readInstructions = do
  input <- readFile "src/day8_instructions.txt"
  return $ fst $ head $ P.readP_to_S instructions input

instructions :: ReadP [Instruction]
instructions = P.endBy1 instruction (P.char '\n') <* P.eof

instruction :: ReadP Instruction
instruction =
  Instruction
    <$> name <* skipSpaces
    <*> operation <* skipSpaces
    <*> number <* skipSpaces
    <* P.string "if" <* skipSpaces
    <*> name <* skipSpaces
    <*> comparison <* skipSpaces
    <*> number

comparison :: ReadP (Int -> Int -> Bool)
comparison = ((<) <$ P.string "<")
  <|> ((>) <$ P.string ">")
  <|> ((<=) <$ P.string "<=")
  <|> ((>=) <$ P.string ">=")
  <|> ((==) <$ P.string "==")
  <|> ((/=) <$ P.string "!=")

operation :: ReadP (Int -> Int -> Int)
operation = ((+) <$ P.string "inc") <|> ((-) <$ P.string "dec")

name :: ReadP String
name = P.many1 $ P.satisfy (\char -> char >= 'a' && char <= 'z')

number :: ReadP Int
number = read <$> ((++) <$> P.option "" (P.string "-") <*> digits)

digits :: ReadP String
digits = P.many1 $ P.satisfy (\char -> char >= '0' && char <= '9')
