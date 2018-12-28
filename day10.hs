import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Number
import qualified Data.Set as S
import Data.List

type Point = (Int, Int)

parser :: Parser [(Point, Point)]
parser = many (vectors <* spaces) <* eof
  where
    vectors = do
        pos <- string "position=<" *> point <* string "> "
        vel <- string "velocity=<" *> point <* string ">"
        return (pos, vel)
    point = do
        a <- many space *> int <* string ","
        b <- many space *> int
        return (a, b)

move :: (Point, Point) -> (Point, Point)
move ((px, py), vel@(vx, vy)) = ((px + vx, py + vy), vel)

tick :: [(Point, Point)] -> [(Point, Point)]
tick = map move

showPoints :: [(Point, Point)] -> String
showPoints points =
    let positions = map fst points
        posSet = S.fromList positions
        xs = map fst positions
        ys = map snd positions
        left = minimum xs
        right = maximum xs
        startRow = minimum ys
        endRow = maximum ys
        gridLines row f
          | row > endRow = []
          | otherwise =
              let line = map (\col -> if f (col, row) then '#' else '.') [left..right]
              in  line : gridLines (row + 1) f
        lines = gridLines startRow ((flip S.member) posSet)
    in intercalate "\n" lines

best input = f 1 (height input) (tick input)
  where
    f n w x =
      let
        w' = height x'
        x' = tick x
      in  if w' > w then (n, x) else f (n+1) w' x'
    height x = let ys = snd . fst <$> x in maximum ys - minimum ys

main = do
    input <- parseFromFile parser "input/day10.txt"
    case input of
        Left err -> print ("Fail: " ++ show err)
        Right x -> let (n, s) = best x in do
            print n
            putStrLn $ showPoints s
