{-# LANGUAGE TupleSections #-}

import           Text.Parsec
import           Text.Parsec.String
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Data.Set                hiding ( splitAt )
import           Control.Monad
import           Data.List                      ( find
                                                , splitAt
                                                )
import           Debug.Trace

parser :: Parser [String]
parser = many identifier <* eof where identifier = many1 letter <* spaces

part1 ids = appearences 2 * appearences 3
  where
    appearences n = length $ mfilter (appear n) $ fmap histogram ids
    appear x m = elem x $ Map.elems m
    histogram = Map.fromListWith (+) . fmap (, 1)

part2 = solve empty
  where
    solve acc (x : xs) = case match x acc of
        (_, Just a ) -> a
        (s, Nothing) -> solve s xs
    match x s =
        let x' = removeOne x in (s `union` fromList x', find (`member` s) x')
    removeOne :: String -> [String]
    removeOne (x : xs) = xs : ((x :) <$> removeOne xs)
    removeOne ""       = []

main = do
    values <- parseFromFile parser "input/day02.txt"
    print $ part1 <$> values
    print $ part2 <$> values
