{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

import           Text.Parsec
import           Text.Parsec.String
import           Text.Parsec.Number

import           Data.List
import           Data.Function
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set

type Point = (Int, Int)
type Bounds = (Point, Point)

parser :: Parser [Point]
parser = many point <* eof
  where
    point = do
        x <- int <* string ", "
        y <- int <* spaces
        return (x, y)

bounds :: [Point] -> Bounds
bounds points = ((minimum xs, minimum ys), (maximum xs, maximum ys))
  where
    xs = fst <$> points
    ys = snd <$> points

allPoints :: Bounds -> [Point]
allPoints ((minx, miny), (maxx, maxy)) = do
    x <- [minx .. maxx]
    y <- [miny .. maxy]
    return (x, y)


distance :: Point -> Point -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

largestArea :: [Point] -> Bounds -> Int
largestArea centers bounds = bruteForce (allPoints bounds) Map.empty
  where
    bruteForce :: [Point] -> Map.Map Int Int -> Int
    bruteForce [] areas =
        maximum $ (areas Map.!) <$> discardInfinites (centers `zip` [0 ..])
    bruteForce (point : points) !areas = bruteForce points $ maybe
        areas
        (\closestPoint -> Map.alter (Just . maybe 1 (+ 1)) closestPoint areas)
        (closest point)
    closest :: Point -> Maybe Int
    closest p =
        let distances = (distance p <$> centers) `zip` [0 ..]
            best      = minimumBy (compare `on` fst) distances
            oneBest   = length (filter ((== fst best) . fst) distances) == 1
        in  if oneBest then Just (snd best) else Nothing

    -- EXTREMELY retarded way to figure out which areas are infinite. Probaly
    -- not correct for all inputs (hah, just increase the checkDistance in that
    -- case)
    discardInfinites :: [(Point, Int)] -> [Int]
    discardInfinites items =
        let infinites :: Set.Set Int
            infinites = foldl' f Set.empty checkpoints
            f !s p = Set.insert (best p) s
            best p = snd (minimumBy (compare `on` (distance p . fst)) items)
            checkDistance = 500
            checkRange    = [-checkDistance .. checkDistance]
            checkpoints =
                ((checkDistance, ) <$> checkRange)
                    ++ ((-checkDistance, ) <$> checkRange)
                    ++ ((, checkDistance) <$> checkRange)
                    ++ ((, -checkDistance) <$> checkRange)
        in  filter (not . (`Set.member` infinites)) (snd <$> items)

safestArea :: [Point] -> Bounds -> Int
safestArea centers bounds = bruteForce
  where
    limit         = 10000
    bruteForce    = length $ filter safeDistance (allPoints bounds)
    safeDistance :: Point -> Bool
    safeDistance p =
        let f _ [] _ = True
            f !accum (x : xs) y =
                let next = accum + distance x y
                in  (next < limit) && f next xs y
        in  f 0 centers p

part1 points = largestArea points $ bounds points
part2 points = safestArea points $ bounds points

main :: IO ()
main = do
    values <- parseFromFile parser "input/day06.txt"
    print $ part1 <$> values
    print $ part2 <$> values

