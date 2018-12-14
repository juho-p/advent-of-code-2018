import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Number

parser :: Parser [Int]
parser = many (int <* spaces) <* eof

data Tree = Tree { treeChildren :: [Tree], treeMetadata :: [Int] }

tree :: [Int] -> (Tree, [Int])
tree (numChildren : numMetadata : items) = (Tree children metadata, remaining)
  where
    childIterations = iterate (\(_,xs) -> tree xs) (tree items)
    children = take numChildren $ fst <$> childIterations
    items' = (items : (snd <$> childIterations)) !! numChildren
    metadata = take numMetadata items'
    remaining = drop numMetadata items'
tree xs = error $ "failed tree " ++ show xs

treeNodes t@(Tree children _) = t : (treeNodes =<< children)

part1 x = sum $ treeMetadata =<< treeNodes x

part2 = value
    where
      value (Tree [] xs) = sum xs
      value (Tree children ixs) = sum $ childValue children <$> ixs
      childValue children i
        | i <= 0 || i > length children = 0
        | otherwise = value $ children !! (i - 1)

main :: IO ()
main = do
    items <- parseFromFile parser "input/day08.txt"
    let root = fst . tree <$> items
    print $ part1 <$> root
    print $ part2 <$> root
