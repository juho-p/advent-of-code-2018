import           Text.Parsec
import           Text.Parsec.String
import           Text.Parsec.Number

import qualified Data.Set                      as Set
import qualified Data.Map.Strict               as Map

newtype Id = Id Int deriving (Show)
newtype Position = Position (Int, Int) deriving (Show, Ord, Eq)
newtype Size = Size (Int, Int) deriving (Show)
data Claim = Claim Id Position Size

parser :: Parser [Claim]
parser = many (claim <* spaces) <* eof
  where
    claim = do
        identifier <- char '#' *> int
        position   <- spaces *> char '@' *> spaces *> pair ','
        size       <- char ':' *> spaces *> pair 'x'
        return $ Claim (Id identifier) (Position position) (Size size)
    pair :: Char -> Parser (Int, Int)
    pair sep = (,) <$> int <* char sep <*> int

places :: Claim -> [Position]
places (Claim _ (Position (x, y)) (Size (w, h))) = do
    dx <- [0 .. w - 1]
    dy <- [0 .. h - 1]
    return $ Position (x + dx, y + dy)

crampedClaims :: [Claim] -> [[Claim]]
crampedClaims = filter ((> 1) . length) . Map.elems . mapOfClaims
    where
        mapOfClaims claims = Map.fromListWith (++) (positions claims)
        positions claims = claims >>= (\x -> places x `zip` repeat [x])

part1 :: [Claim] -> Int
part1 = length . crampedClaims

part2 claims = identifier loneClaim
  where
    identifier (Claim (Id i) _ _) = i
    ids        = Set.fromList $ identifier <$> concat (crampedClaims claims)
    loneClaims = filter (not . (`Set.member` ids) . identifier) claims
    loneClaim  = head loneClaims

main :: IO ()
main = do
    values <- parseFromFile parser "input/day03.txt"
    print $ part1 <$> values
    print $ part2 <$> values
