import Prelude hiding (length, splitAt)
import qualified Data.Map.Strict as M
import qualified Data.List.PointedList as PL
import qualified Data.List.PointedList.Circular as Circular

normalTurn circle nextMarble =
    PL.insertRight nextMarble $ Circular.next circle

specialTurn circle =
    let circle' = Circular.moveN (-7) circle
    in case Circular.deleteRight $ circle' of
        Just x -> (x, PL._focus circle')
        Nothing -> error "list is somehow empty"

takeTurn :: Int -> Int -> M.Map Int Int -> PL.PointedList Int -> (M.Map Int Int, PL.PointedList Int)
takeTurn player nextMarble score circle
    | nextMarble `mod` 23 == 0 =
        let (circle', val) = specialTurn circle
        in ((M.alter (updateScore $ val + nextMarble) player score), circle')
    | otherwise = (score, normalTurn circle nextMarble)
    where updateScore x old = Just $ maybe x (x+) old

highScore playerCount lastMarble = run 0 M.empty (PL.singleton 0)
  where
    run stepNum score circle 
        | stepNum < lastMarble = run nextMarble score' circle'
        | otherwise = maximum $ M.elems score
        where
          (score', circle') = takeTurn (player stepNum) nextMarble score circle 
          nextMarble = stepNum + 1
          player = (`mod` playerCount)

main = do
    print $ highScore 410 72059
    print $ highScore 410 7205900
