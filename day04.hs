import           Text.Parsec
import           Text.Parsec.String
import           Text.Parsec.Number

import           Data.List
import           Data.Function
import qualified Data.Map.Strict               as Map

data Event = Sleep | Wake | Begin Int deriving (Show)
type Entry = (Timestamp, Event)
data Timestamp = Timestamp
    { year :: Int
    , month :: Int
    , day :: Int
    , hour :: Int
    , minute :: Int } deriving (Ord, Eq, Show)

parser :: Parser [Entry]
parser = many (entry <* spaces) <* eof
  where
    entry = do
        timestamp <-
            Timestamp
            <$> (char '[' *> int)
            <*> (char '-' *> int)
            <*> (char '-' *> int)
            <*> (spaces *> int)
            <*> (char ':' *> int <* char ']' <* spaces)
        event <- try sleep <|> try wake <|> try begin
        return (timestamp, event)
    sleep = const Sleep <$> string "falls asleep"
    wake  = const Wake <$> string "wakes up"
    begin = Begin <$> (string "Guard #" *> int <* string " begins shift")

strategies entries = solve Map.empty 0 0 entries
  where
    solve :: Map.Map Int [Int] -> Int -> Int -> [Entry] -> (Int, Int)
    solve sleeps _      _     []                   = solution sleeps
    solve sleeps active start ((ts, event) : rest) = case event of
        Begin x -> solve sleeps x start rest
        Sleep   -> solve sleeps active (minute ts) rest
        Wake    -> solve (update sleeps active [start .. (minute ts - 1)])
                         active
                         start
                         rest
    update m guard minutes = Map.insertWith (flip (++)) guard minutes m
    mostFrequent xs =
        let counts = Map.fromListWith (+) $ xs `zip` repeat 1
        in  maximumBy (compare `on` snd) $ Map.toList counts
    solution m =
        let sleepMinutes = Map.toList m
            (lazyGuard, lazyMinutes) =
                maximumBy (compare `on` (length . snd)) sleepMinutes
            strategy1 = lazyGuard * fst (mostFrequent lazyMinutes)
            (reliableGuard, (reliableMinute, _)) = maximumBy
                (compare `on` (snd . snd))
                (map (\(g, xs) -> (g, mostFrequent xs)) sleepMinutes)
            strategy2 = reliableGuard * reliableMinute
        in  (strategy1, strategy2)


main :: IO ()
main = do
    values <- parseFromFile parser "input/day04.txt"
    let sorted   = sortOn fst <$> values
    let solution = strategies <$> sorted
    print $ fst <$> solution
    print $ snd <$> solution
