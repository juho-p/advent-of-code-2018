{-# LANGUAGE BangPatterns #-}

import Data.Char
import Data.Foldable (toList)
import Data.List
import Data.Sequence ((|>), index, Seq, fromList)
import qualified Data.Sequence as S

import Debug.Trace

initialScores = [3, 7]

initialElves = (0, 1)

type Elves = (Int, Int)
type Scores = Seq Int

allScores :: [Int]
allScores = initialScores ++ go initialElves (fromList initialScores)
  where
    go (!a, !b) !scores
      | newRecipes < 10 = newRecipes : go elves' scores'
      | otherwise = scoreA : scoreB : go elves' scores'
      where
        scores'
          | newRecipes < 10 = scores |> newRecipes
          | otherwise = scores |> scoreA |> scoreB
        newRecipes = index scores a + index scores b
        (scoreA, scoreB) = newRecipes `divMod` 10
        elves' = (nextPlace a, nextPlace b)
        length' = if newRecipes < 10 then S.length scores + 1 else S.length scores + 2
        nextPlace x = ((scores `index` x) + x + 1) `mod` length'

part1 :: Int -> String
part1 n = (chr . (+ ord '0')) <$> (take 10 $ drop n allScores)

part2 :: [Int] -> Maybe Int
part2 target = elemIndex target $ (take $ length target) <$> tails allScores

main :: IO ()
main = do
    input <- read <$> readFile "input/day14.txt"
    print $ part1 input
    print $ part2 (((ord '0' `subtract`) . ord) <$> show input)
