import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Number

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Bits

parser :: Parser ([([Int], [Int], [Int])], [Int])
parser = (,) <$> (many spec) <*> (many1 (int <* spaces)) where
    spec = do
        string "Before:" *> spaces
        before <- intlist <* spaces

        params <- many1 (int <* spaces)

        string "After:" *> spaces
        after <- intlist <* spaces

        return (before, params, after)

    intlist = read <$> many1 (noneOf "\n") :: Parser [Int]

type Registers = M.Map Int Int
type Operands = [Int]

data Opcode = Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori | Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr deriving (Eq, Ord, Enum, Show)

evaluateInstruction :: Opcode -> Operands -> Registers -> Registers
evaluateInstruction op (a:b:c:[]) registers = case op of
    Addr -> storeR (+)
    Addi -> storeI (+)
    Mulr -> storeR (*)
    Muli -> storeI (*)
    Banr -> storeR (.&.)
    Bani -> storeI (.&.)
    Borr -> storeR (.|.)
    Bori -> storeI (.|.)
    Setr -> store $ reg a
    Seti -> store a
    Gtir -> cmp (>) a (reg b)
    Gtri -> cmp (>) (reg a) b
    Gtrr -> cmp (>) (reg a) (reg b)
    Eqir -> cmp (==) a (reg b)
    Eqri -> cmp (==) (reg a) b
    Eqrr -> cmp (==) (reg a) (reg b)
  where
    store x = M.insert c x registers
    storeR f = store $ reg a `f` reg b
    storeI f = store $ reg a `f` b
    cmp f x y = store $ if x `f` y then 1 else 0
    reg x = registers M.! x

opcodes :: [Opcode]
opcodes = enumFrom Addr

validateOpcode :: Operands -> Registers -> Registers -> Opcode -> Bool
validateOpcode params before after op = y == after
    where y = evaluateInstruction op params before

matchingOpcodes :: Operands -> Registers -> Registers -> [Opcode]
matchingOpcodes params before after = filter (validateOpcode params before after) opcodes

registers :: [Int] -> M.Map Int Int
registers xs = M.fromList $ [0..] `zip` xs

part1 xs = length $ filter (>= 3) (map (length . matches) xs)
    where matches (a,b,c) = matchingOpcodes (tail b) (registers a) (registers c)

analyzeInstruction
    :: M.Map Int (S.Set Opcode)
    -> Int
    -> Operands
    -> Registers
    -> Registers
    -> M.Map Int (S.Set Opcode)
analyzeInstruction m i params before after = M.insertWith S.intersection i matches m
    where matches = S.fromList $ matchingOpcodes params before after

mapInstructions input = resolve $ analyze M.empty input where
    -- FIXME: this could actually get stuck in infinite loop so it's bad code
    resolve m
        | any ((== 0) . S.size) $ M.elems m = error "something went terribly wrong"
        | length singletons == M.size m = M.map (head . S.elems) m
        | otherwise = resolve $ M.map dropResolved m
        where
          singletons = filter ((== 1) . S.size) $ M.elems m
          resolved = foldl S.union S.empty singletons
          dropResolved s = if S.size s == 1 then s else S.difference s resolved
    analyze m [] = m
    analyze m ((before,(opc:params),after):xs) = analyze m' xs
        where m' = analyzeInstruction m opc params (registers before) (registers after)

runProgram regs _ [] = regs
runProgram regs m (o:a:b:c:xs) = runProgram (evaluateInstruction (m M.! o) [a,b,c] regs) m xs

main :: IO ()
main = do
    let regs = M.fromList $ [0..3] `zip` repeat 0
    input <- parseFromFile parser "input/day16.txt"
    case input of
        Left err -> print err
        Right (samples, program) -> do
            print $ part1 samples
            let result = runProgram regs (mapInstructions samples) program
            print $ result M.! 0
