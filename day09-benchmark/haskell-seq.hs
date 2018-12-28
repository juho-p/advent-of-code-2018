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

main = do
    print $ highScore 410 72059
    print $ highScore 410 7205900
