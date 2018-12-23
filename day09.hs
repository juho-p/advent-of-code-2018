import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Number

import Prelude hiding (length, splitAt)
import Data.Sequence
import qualified Data.Map.Strict as M

normalTurn (first :<| second :<| tail) next =
    (next <| tail) |> first |> second

specialTurn circle =
    let removeIdx = length circle - 7
        (left, (removeVal :<| right)) = splitAt removeIdx circle
        circle' = right >< left
    in (circle', removeVal)

takeTurn :: Int -> Int -> M.Map Int Int -> Seq Int -> (M.Map Int Int, Seq Int)
takeTurn player next score circle
    | next `mod` 23 == 0 =
        let (circle', val) = specialTurn circle
        in ((M.alter (updateScore $ val + next) player score), circle')
    | otherwise = (score, normalTurn circle next)
    where updateScore x old = Just $ maybe x (x+) old

highScore playerCount lastMarble = run 1 M.empty (fromList [1, 0])
  where
    run stepNum score circle 
        | stepNum < lastMarble = run next score' circle'
        | otherwise = maximum $ M.elems score
        where
          (score', circle') = takeTurn (player stepNum) next score circle 
          next = stepNum + 1
          player = (`mod` playerCount)

parser :: Parser (Int, Int)
parser = (,)
    <$> int
    <*> (string " players; last marble is worth " *> int <* string " points")

part1 :: (Int, Int) -> Int
part1 (c, n) = highScore c n

part2 (c, n) = highScore c (n * 100)

main = do -- >122927
    input <- parseFromFile parser "input/day09.txt"
    print $ part1 <$> input
    print $ part2 <$> input
