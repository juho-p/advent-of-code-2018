{-# LANGUAGE BangPatterns #-}

import Text.Parsec
import Text.Parsec.String

import Data.List
import qualified Data.Set as Set

newtype Note = Note Int deriving (Show)
type NoteTable = Set.Set Int

data Pot = Pot { potNumber :: Int, hasPlant :: Bool } deriving (Show, Eq, Ord)

type Input = ([Bool], [([Bool], Bool)])
parser :: Parser Input
parser = (,)
    <$> (string "initial state: " *> pots <* spaces)
    <*> many ((,)
        <$> (pots <* string " => ")
        <*> pot
        <* spaces)
    <* eof
    where
        pots = many pot
        pot = (=='#') <$> oneOf ".#"

containsPlant :: NoteTable -> Note -> Bool
containsPlant s (Note x) = x `Set.member` s

note :: [Bool] -> Note
note xs = Note value
  where
    value = sum $ zipWith f xs [2^n | n <- [0..4] :: [Int]]
    f b x = if b then x else 0

noteTable :: [Note] -> NoteTable
noteTable xs = Set.fromList ((\(Note x) -> x) <$> xs)

spread :: NoteTable -> [Pot] -> [Pot]
spread _ [] = [] -- our garden would explode with rules where this weren't the case!
spread notes pots@(Pot first _:_) = simplify result
  where
    result = zipWith Pot [first-2 ..] plants'
    plants = replicate 4 False ++ (hasPlant <$> pots) ++ repeat False
    plants' = take (length pots + 8) $ containsPlant notes <$> patterns
    patterns = note <$> tails plants
    simplifyL = dropWhile (not . hasPlant)
    simplify xs = reverse (simplifyL (reverse (simplifyL xs)))

solve :: Int -> Input -> Int
solve target (start, rules) = iter (spread notes) [] startState target
  where
    iter :: ([Pot] -> [Pot]) -> [Pot] -> [Pot] -> Int -> Int
    iter f x' !x n
        | (hasPlant <$> x') == (hasPlant <$> x) = quickSolution f x n
        | n > 0 = iter f x (f x) (n - 1)
        | otherwise = score x
    quickSolution f x n = (score (f x) - score x) * n + score x
    notes = noteTable [note a | (a,b) <- rules, b]
    startState = zipWith Pot [0..] start
    score xs = sum ((\(Pot i b) -> if b then i else 0) `map` xs)

main :: IO ()
main = do
    input <- parseFromFile parser "input/day12.txt"
    print $ solve 20 <$> input
    print $ solve 50000000000 <$> input
