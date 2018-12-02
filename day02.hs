{-# LANGUAGE TupleSections #-}

import           Text.Parsec
import           Text.Parsec.String
import           Data.Maybe
import           Control.Monad
import           Data.List
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as S

parser :: Parser [String]
parser = many identifier <* eof where identifier = many1 letter <* spaces

part1 :: [String] -> Int
part1 ids = appearences 2 * appearences 3
  where
    appearences :: Int -> Int
    appearences n = length $ mfilter (appear n) $ fmap histogram ids
    appear x m = elem x $ Map.elems m
    histogram = Map.fromListWith (+) . fmap (, 1)

part2 :: [String] -> String
part2 ids = head solutions
  where
    solutions = (maybeToList . findNonUnique S.empty) =<< idLists
    idLists   = transpose $ removeOne <$> ids
    removeOne :: String -> [String]
    removeOne (x : xs) = xs : ((x :) <$> removeOne xs)
    removeOne ""       = []
    findNonUnique :: S.Set String -> [String] -> Maybe String
    findNonUnique acc (x : xs) | S.member x acc = Just x
                               | otherwise = findNonUnique (S.insert x acc) xs
    findNonUnique _ [] = Nothing

main :: IO ()
main = do
    values <- parseFromFile parser "input/day02.txt"
    print $ part1 <$> values
    print $ part2 <$> values
