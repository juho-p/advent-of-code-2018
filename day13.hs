import qualified Data.Map.Strict as M
import Data.List
import Data.Tuple

data Track = Straight | Intersection | Forward | Backward deriving (Show, Ord, Eq)

data Cart = Up | Rgt | Dwn | Lft deriving (Show, Ord, Eq, Enum)

type ElfState = (M.Map (Int, Int) Track, [((Int, Int), Int, Cart)])

readMap :: String -> ElfState
readMap input = readMap' 0 0 input M.empty [] where
    readMap' _ _ "" a b = (a, b)
    readMap' row col (c : str) tracks carts =
        case c of
            '^' -> cart Up
            '>' -> cart Rgt
            'v' -> cart Dwn
            '<' -> cart Lft
            '|' -> track Straight
            '-' -> track Straight
            '+' -> track Intersection
            '/' -> track Forward
            '\\' -> track Backward
            otherwise -> next tracks carts
          where
            next = readMap' row' col' str
            (row', col') = if c == '\n' then (row + 1, 0) else (row, col + 1)
            pos = (row, col)
            cart x = next (M.insert pos Straight tracks) ((pos, 0, x) : carts)
            track x = next (M.insert pos x tracks) carts

advance :: ElfState -> ElfState
advance (tracks, carts) = (tracks, map moveAndTurn carts) where
    moveAndTurn ((row, col), intersectionCount, cart) =
        let pos' = move row col cart
            (n, cart') = turn (tracks M.! pos') intersectionCount cart
        in (pos', n, cart')
    turn t n x = ((if t == Intersection then n+1 else n),
        case t of
            Straight -> x
            Intersection -> case n `mod` 3 of
                2 -> rotate x
                1 -> x
                0 -> opposite $ rotate x
            Forward -> curve x
            Backward -> opposite $ curve x)
    findPair table x = snd . head $ filter ((==x) . fst) (table ++ (swap <$> table))
    opposite = findPair [(Up, Dwn), (Lft, Rgt)]
    curve = findPair [(Up, Rgt), (Dwn, Lft)]
    rotate x = if x == Lft then Up else succ x

move r c x = case x of
    Up  -> (r-1, c)
    Rgt -> (r, c+1)
    Dwn -> (r+1, c)
    Lft -> (r, c-1)

findCrash :: ElfState -> Maybe (Int, Int)
findCrash (_, carts) = f [] (sort carts) where
    f _ [] = Nothing
    f acc ((pos@(row,col),_,cart):xs)
      | elem pos acc = Just pos
      | elem pos' acc = Just pos'
      | otherwise = f (pos':acc) xs
      where pos' = move row col cart

part1 state = case findCrash state of
    Just x -> x
    Nothing -> part1 $ advance state

-- crap code but whatever
resolveCrashes :: ElfState -> ElfState
resolveCrashes state@(_, carts) = f [] state (sort carts) where
    f _ x [] = x
    f acc state'@(trackmap, cartlist) ((pos@(row,col),_,cart):xs)
      | isCollision pos = collide pos pos
      | isCollision pos' = collide pos pos'
      | otherwise = f ((pos,pos'):acc) state' xs
      where
        pos' = move row col cart
        isCollision x = elem x (map snd acc)
        collide p x = f (filter ((/= x) . snd) acc) (trackmap, cartlist' p x) xs
        cartlist' p x = filter (\(p',_,_) -> not $ elem p' removed) cartlist where removed = (removed' p x)
        removed' p x = p : (fst <$> filter ((==x) . snd) acc)

part2 :: ElfState -> (Int,Int)
part2 state@(_, ((p@(r,c),_,x):carts))
    | null carts = move r c x
    | otherwise = part2 $ resolveCrashes (advance state)

main :: IO ()
main = do
    input <- readFile "input/day13.txt"
    let state = readMap input
    let (y, x) = part1 state
    putStrLn $ (show x) ++ "," ++ (show y)
    let (y, x) = part2 state
    putStrLn $ (show x) ++ "," ++ (show y)
