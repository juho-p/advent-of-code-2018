{-# LANGUAGE BangPatterns #-}

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Number

import Data.Tuple
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Foldable

type Point = (Int, Int)

parser :: Parser [Point]
parser = concat <$> (many slice <* eof)
  where
    slice = vertical <|> horizontal
    vertical = numbers 'x' 'y'
    horizontal = (map swap) <$> numbers 'y' 'x'
    numbers :: Char -> Char -> Parser [(Int,Int)]
    numbers a b = do
        first <- char a *> char '=' *> int
        second <- string ", " *> char b *> char '=' *> int
        third <- string ".." *> int <* spaces
        return [(first, y) | y <- [second..third]]

data Tile = Clay | SettledWater | FlowingWater deriving (Eq, Show)

settledWater word (x,y) =
    case (lefts, rights) of
        (Just xs, Just ys) -> Just (Set.fromList (xs ++ ys))
        _ -> Nothing
    where
        lefts = settledTiles $ takeWhile onPlatform [(x', y) | x' <- [x,x-1..]]
        rights = settledTiles $ takeWhile onPlatform [(x', y) | x' <- [x..]]
        settledTiles xs = if any isClay xs then Just (takeWhile (not . isClay) xs) else Nothing
        isClay (x,y) = Map.lookup (x, y) word == Just Clay
        onPlatform (x,y) = case Map.lookup (x,y+1) word of
            Just Clay -> True
            Just SettledWater -> True
            _ -> False

step :: Int -> Map Point Tile -> [Point] -> (Map Point Tile, [Point])
step maxDepth !word active@((!x, !y):nextActive)
    | y >= maxDepth = (word, nextActive)
    | isSand down = flow down
    | Map.lookup down word == Just FlowingWater = (word, nextActive)
    | isSand left = flow left
    | isSand right = flow right
    | isSettled =
        let
            word' = Map.union (Map.fromList $ zip (toList settled) (repeat SettledWater)) word
            active' = dropWhile (`Set.member` settled) nextActive
        in (word', active')
    | otherwise = (word, nextActive)
    where
        flow target = (Map.insert target FlowingWater word, target : active)
        isSand p = not $ p `Map.member` word
        (isSettled, settled) = case settledWater word (x, y) of
            Just xs -> (True, xs)
            Nothing -> (False, Set.empty)
        left = (x-1, y)
        right = (x+1, y)
        down = (x, y+1)

evalWorld _ w [] = w
evalWorld maxDepth word active = evalWorld maxDepth word' active'
    where (word', active') = step maxDepth word active

completeWorld input = evalWorld (maximum $ snd <$> input) word [(500,0)]
    where word = Map.fromList (input `zip` repeat Clay)

solve x = (Map.size waters, Map.size settledWaters)
    where
        waters = Map.filterWithKey valid $ completeWorld x
        settledWaters = Map.filter (== SettledWater) waters
        valid (_,y) t = t /= Clay && (y >= limit)
        limit = minimum (snd <$> x)

main :: IO ()
main = do
    input <- parseFromFile parser "input/day17.txt"
    let solution = solve <$> input
    print $ fst <$> solution
    print $ snd <$> solution
