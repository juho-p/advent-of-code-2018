import           Data.Char
import           Data.Function

processedLength :: String -> Int
processedLength input = (length . snd . head)
    $ dropWhile (uncurry ((/=) `on` length)) (iterations `zip` tail iterations)
  where
    iterations = tail $ iterate process input
    process :: String -> String
    process = process' [] Nothing
    process' accum Nothing [] = accum
    process' accum (Just prev) [] = process' (prev : accum) Nothing []
    process' accum Nothing (curr : units) = process' accum (Just curr) units
    process' accum (Just prev) (curr : units)
        | isReaction prev curr = process' accum Nothing units
        | otherwise            = process' (prev : accum) (Just curr) units
    isReaction x y = toUpper x == toUpper y && x /= y

part1 = processedLength

part2 input = minimum $ processedLength <$> ((`remove` input) <$> ['a' .. 'z'])
    where remove c = filter ((c /=) . toLower)

main :: IO ()
main = do
    input <- filter (not . isSpace) <$> readFile "input/d05.txt"
    print $ part1 input
    print $ part2 input
