import           Text.Parsec
import           Text.Parsec.String
import           Text.Parsec.Number
import           Data.Set

parser :: Parser [Int]
parser = many (int <* spaces) <* eof

part1 :: [Int] -> Int
part1 = sum

part2 :: [Int] -> Int
part2 = solve empty 0 . cycle
  where
    solve reached latest (delta : tail) =
        let next = latest + delta
        in  if member next reached
                then next
                else solve (insert next reached) next tail

main = do
    values <- parseFromFile parser "input/day01.txt"
    print $ part1 <$> values
    print $ part2 <$> values
