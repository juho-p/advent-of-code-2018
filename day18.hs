{-# LANGUAGE BangPatterns #-}

import qualified Data.Array as A
import Data.Array ((!), array)
import Data.Foldable
import qualified Data.Map.Strict as M

data Space = Open | Trees | Lumberyard deriving (Show, Eq, Ord)

type Spaces = A.Array Int Space

width = 50
height = 50

at :: (Int,Int) -> Spaces -> Maybe Space
at (x,y) spaces
    | x < 0 || y < 0 = Nothing
    | x >= width || y >= height = Nothing
    | otherwise = Just $ spaces ! (y * width + x)

evolve :: [Space] -> Space -> Space
evolve !adjacent !space
    | space == Open && count Trees >= 3 = Trees
    | space == Trees && count Lumberyard >= 3 = Lumberyard
    | space == Lumberyard =
        if any (== Lumberyard) adjacent && any (== Trees) adjacent then Lumberyard
        else Open
    | otherwise = space
    where count x = length $ filter (==x) adjacent

neighbors :: Spaces -> (Int,Int) -> [Space]
neighbors spaces (x,y) = concatMap toList maybes
    where maybes = (`at` spaces) `map`
            [ (x',y')
            | x' <- [x-1..x+1]
            , y' <- [y-1..y+1]
            , (x,y) /= (x',y')
            ]

readChar :: Char -> Space
readChar '.' = Open
readChar '|' = Trees
readChar '#' = Lumberyard
readChar c = error $ "unknown space: " ++ [c]

spacesFromList xs = array (0,width*height - 1) ([0..] `zip` xs)

readInput :: String -> Spaces
readInput input = spacesFromList $ readChar <$> (take width =<< (take height $ lines input))

step !spaces = spacesFromList spaces'
  where
    spaces' = evolve' <$> points
    points = [(x,y) | y <- [0..height-1], x <- [0..width-1]]
    evolve' p@(x,y) = evolve (neighbors spaces p) (spaces ! (x + y*width))

nthStep 0 x = x
nthStep n spaces = nth M.empty n spaces
  where
    nth _ 0 x = x
    nth m !n !spaces
      | M.member spaces m = nth M.empty quickN spaces
      | otherwise = nth (M.insert spaces n m) (n-1) (step spaces)
      where quickN = n `mod` ((m M.! spaces) - n)

resourceValue spaces = count Trees * count Lumberyard
  where
    count x = length $ filter (==x) list
    list = toList spaces

part1 spaces = resourceValue $ nthStep 10 spaces
part2 spaces = resourceValue $ nthStep 1000000000 spaces

main :: IO ()
main = do
    input <- readFile "input/day18.txt"
    let spaces = readInput input
    print $ part1 spaces
    print $ part2 spaces
