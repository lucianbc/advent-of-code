{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use second" #-}
{-# HLINT ignore "Use list comprehension" #-}

import Control.Applicative qualified as Map
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as S
import Data.Text (pack, splitOn, unpack)
import Debug.Trace
import GHCi.Run (run)
import System.Environment

main = do
  lines <- readInput
  part <- readPart
  let result = case part of
        "part1" -> part1 lines
        "part2" -> part2 lines
        _ -> "Bad part argument. Must be \"part1\" or \"part2\""
  putStrLn $ "Result is " ++ result

part1 :: [String] -> String
part1 lines =
  let (pairs, numberLines) = parseInput lines
      tagged = zipWith (\a b -> (a, isOrderCorrect pairs b)) numberLines numberLines
      result = sum $ do
        (nums, isOk) <- tagged
        if isOk then return (nums !! (length nums `div` 2)) else []
   in show result

isOrderCorrect :: [(Int, Int)] -> [Int] -> Bool
isOrderCorrect orderingPairs line =
  let pairs = generatePairs line
      isPairOk = checkPair orderingPairs <$> pairs
   in and isPairOk

checkPair :: [(Int, Int)] -> (Int, Int) -> Bool
checkPair [] _ = False
checkPair ((a, b) : ps) (x, y)
  | a == x && b == y = True
  | a == y && b == x = False
  | otherwise = checkPair ps (x, y)

part2 :: [String] -> String
part2 _ =
  let edges = [(47, 53), (97, 61), (97, 47), (75, 53), (61, 53), (97, 53), (75, 47), (97, 75), (47, 61), (75, 61)]
   in show $ sortNodes edges

-- part2 lines =
--   let (edges, numberLines) = parseInput lines
--       notOkLines = do
--         pageLine <- numberLines
--         let isInOrder = isOrderCorrect edges pageLine
--         if isInOrder then [] else [pageLine]
--       x = do
--         notOkLine <- notOkLines
--         let notOkSet = S.fromList $ traceShowId notOkLine
--         let edgesToConsider = filter (\(a, b) -> a `S.member` notOkSet && b `S.member` notOkSet) edges
--         let sorted = sortNodes edgesToConsider
--         -- let graph = traceShowId $ createInitialStep edgesToConsider
--         return sorted
--    in -- let edgesToConsider = filter (\(a, b) -> a `S.member` notOkSet && b `S.member` notOkSet) edges
--       -- let edgesToConsider2 = trace ("edges to consider " ++ show edgesToConsider) edgesToConsider
--       -- let graph = createInitialStep edgesToConsider2
--       -- let graph2 = trace ("Graph is " ++ show graph) graph
--       -- let sorted = runTopologicalSort graph2
--       -- return sorted
--       -- mids = do
--       --   p <- orderedNotOk
--       --   return (p !! (length p `div` 2))
--       --  in show $ sum mids
--       show x

sortNodes :: [(Int, Int)] -> [Int]
sortNodes edges =
  let initialStep = traceShowId $ createInitialStep edges
      sorted = runTopologicalSort initialStep
   in trace "Sort nodes" sorted

myTrace :: (Show a) => String -> a -> a
myTrace prepend x = trace (prepend ++ " " ++ show x) x

data TopologicalStep = TopologicalStep
  { edges :: [(Int, Int)],
    incomingDegree :: [(Int, Int)],
    sorted :: [Int]
  }

instance Show TopologicalStep where
  show (TopologicalStep e d s) = "Edges: " ++ show e ++ "\n" ++ "Degrees: " ++ show d ++ "\nSorted: " ++ show s

createInitialStep :: [(Int, Int)] -> TopologicalStep
createInitialStep edges =
  let allNodes = Map.fromList $ map (,0 :: Int) (edges >>= (\(a, b) -> [a, b]))
      degreesMap = foldl (\acc (from, to) -> Map.adjust (+ 1) to acc) allNodes edges
   in TopologicalStep edges (Map.assocs degreesMap) []

runTopologicalSort :: TopologicalStep -> [Int]
runTopologicalSort step =
  let oneMoreTurn = topologicalSort step
   in case oneMoreTurn of
        Just nextTurn -> runTopologicalSort nextTurn
        Nothing -> reverse . sorted $ step

adjust :: (Eq k) => (v -> v) -> k -> [(k, v)] -> [(k, v)]
adjust fn k = map (\(a, b) -> if a == k then (a, fn b) else (a, b))

topologicalSort :: TopologicalStep -> Maybe TopologicalStep
topologicalSort step = do
  candidate <- findCandidateNode . incomingDegree $ step
  let nodesPointedByCandidate = do
        (from, to) <- edges step
        if from == candidate then return to else []
  let newDegrees = do
        (node, degree) <- incomingDegree step
        if node == candidate
          then []
          else
            if node `elem` nodesPointedByCandidate && degree >= 0
              then [(node, degree - 1)]
              else [(node, degree)]
  return step

findCandidateNode :: [(Int, Int)] -> Maybe Int
findCandidateNode incomingDegree =
  fst <$> L.find ((0 ==) . snd) incomingDegree

generatePairs :: [Int] -> [(Int, Int)]
generatePairs [] = []
generatePairs [_] = []
generatePairs (x : xs) =
  let tailPairs = generatePairs xs
      headPairs = do
        y <- xs
        return (x, y)
   in headPairs ++ tailPairs

type InputData = ([(Int, Int)], [[Int]])

parseInput :: [String] -> InputData
parseInput ("" : xs) = parseRows xs
  where
    parseRows :: [String] -> InputData
    parseRows [] = ([], [])
    parseRows (x : xs) =
      let (pairs, rows) = parseRows xs
          nums = map unpack $ splitOn "," $ pack x
          numsInt = map (read :: String -> Int) nums
       in (pairs, numsInt : rows)
parseInput (x : xs) =
  let (pairs, rows) = parseInput xs
      [a, b] = map ((read :: String -> Int) . unpack) $ splitOn "|" $ pack x
   in ((a, b) : pairs, rows)

-- parseInput ("" : xs) = parseRows xs
--   where
--     parseRows :: [String] -> InputData -> InputData
--     parseRows [] input = input
--     parseRows (x:xs) (pairs, rows) =

readPart :: IO String
readPart = do
  args <- getArgs
  return $ args !! 1

readInput :: IO [String]
readInput = do
  args <- getArgs
  let filePath = head args
  inputContent <- readFile filePath
  return $ lines inputContent
