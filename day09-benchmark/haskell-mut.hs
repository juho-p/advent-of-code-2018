import Data.Mutable
import Data.Foldable
import Data.IORef
import Control.Monad
import qualified Data.Map as M

type IntDeque = SDeque RealWorld Int

deque :: IO IntDeque
deque = newColl

normalTurn :: IntDeque -> Int -> IO ()
normalTurn v next = do
    first <- popFront v
    second <- popFront v
    pushFront v next
    for_ first $ pushBack v
    for_ second $ pushBack v
    return ()

specialTurn :: IntDeque -> IO Int
specialTurn v = do
    reverseTail <- concatMap toList <$> (replicateM 6 $ popBack v)
    removeVal <- sum <$> popBack v
    for_ reverseTail $ pushFront v
    return removeVal

takeTurn :: Int -> Int -> IORef (M.Map Int Int) -> IntDeque -> IO ()
takeTurn player next score v =
    if next `mod` 23 == 0
        then do
            removed <- specialTurn v
            s <- readIORef score
            writeIORef score $
                let x = removed + next
                in M.alter (\old -> Just $ maybe x (x+) old) player s
        else normalTurn v next

highScore :: Int -> Int -> IO Int
highScore playerCount lastMarble = do
    score <- newIORef M.empty
    circle <- deque
    pushBack circle 1
    pushBack circle 0

    let players = cycle [1..playerCount]
    let marbles = [2..lastMarble]
    let actions = zipWith takeTurn players marbles

    forM_ (players `zip` marbles) (\(p, m) -> takeTurn p m score circle)

    finalScore <- readIORef score

    return (maximum $ M.elems finalScore)

main :: IO ()
main = do
    part1 <- highScore 410 72059
    print part1
    part2 <- highScore 410 7205900
    print part2
