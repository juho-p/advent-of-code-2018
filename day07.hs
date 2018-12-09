{-# LANGUAGE BangPatterns #-}

import Text.Parsec
import Text.Parsec.String

import Data.Char
import Data.Maybe
import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

parser :: Parser [(Char,Char)]
parser = many dependency <* eof
    where
        dependency = (,) <$>
            (string "Step " *> letter) <*>
            (string " must be finished before step " *> letter <* string " can begin." <* spaces)

allSteps :: String
allSteps = ['A' .. 'Z']

assemblyOrder :: Set.Set Char -> Set.Set (Char,Char) -> String
assemblyOrder steps deps
    | Set.null deps = Set.toList steps
    | otherwise = nextStep :
            assemblyOrder
                (nextStep `Set.delete` steps)
                (Set.filter ((/=nextStep) . fst) deps)
        where
            nextStep = minimum $
                Set.filter (not . (`Set.member` Set.map snd deps)) steps

type Worker = (Int, Maybe Char)

assemblyDuration :: Int -> Map.Map Char String -> String -> [Worker] -> Int
assemblyDuration !acc deps steps workers
  | null steps && all (isNothing . snd) workers = acc
  | canStartTask = assemblyDuration acc deps steps' workers'
  | otherwise = assemblyDuration (acc + 1) deps' steps workers'
    where
      canStartTask
        | null steps = False
        | otherwise = any isFreeWorker workers && isJust nextStep
      nextStep = find (not . (`Map.member` deps)) steps
      isFreeWorker = (== 0) . fst
      steps' = maybe steps (`delete` steps) nextStep
      workers'
        | canStartTask = maybe workers (assignTask [] workers) nextStep
        | otherwise = advance <$> workers
      assignTask acc (worker:workers) step
        | isFreeWorker worker = (stepDuration step, Just step) : acc ++ workers
        | otherwise = assignTask (worker:acc) workers step
      stepDuration step = ord step - ord 'A' + 61
      advance (time, task)
        | time <= 1 = (0, Nothing)
        | otherwise = (time - 1, task)
      deps' = Map.filter (not . null) $ Map.map (\\ completedSteps) deps
      completedSteps = [fromMaybe '0' x | (t, x) <- workers, t == 1]

part1 = (assemblyOrder $ Set.fromList allSteps) . Set.fromList

part2 input = assemblyDuration 0 deps (part1 input) (makeWorkers 5)
    where deps = Map.fromListWith (++) [(y,[x]) | (x,y) <- input]

makeWorkers n = take n $ repeat (0, Nothing)

main :: IO ()
main = do
    deps <- parseFromFile parser "input/day07.txt"
    print $ part1 <$> deps
    print $ part2 <$> deps
