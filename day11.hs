import Data.List
import Data.Function

type Point = (Int, Int)

powerLevel :: Int -> Point -> Int
powerLevel serial (x, y) =
    let rackId = x + 10
        level = (rackId * y + serial) * rackId
    in  ((level `mod` 1000) `div` 100) - 5

gridPowerLevel :: Int -> Int -> Int -> Point -> Int
gridPowerLevel serial width height (x, y) = sum
    [ powerLevel serial (x', y')
    | x' <- take width [x..]
    , y' <- take height [y..]
    ]

part1 :: Int -> (Int,Int)
part1 serial = maximumBy (compare `on` gridPowerLevel serial 3 3)
    [ (x, y)
    | x <- [1..298]
    , y <- [1..298]
    ]

-- TODO: more optimal solution
part2 :: Int -> (Int,Int,Int)
part2 serial = maximumBy (compare `on` level)
    [ (x, y, z)
    | x <- [1..298]
    , y <- [1..298]
    , z <- [1..20]
    ]
    where level (x, y, z) = gridPowerLevel serial z z (x, y)

main :: IO ()
main = do
    let printSolution x = print $ filter (`notElem` "()") (show x)
    serial <- read <$> readFile "input/day11.txt"
    printSolution $ part1 serial
    printSolution $ part2 serial
