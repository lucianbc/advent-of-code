{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

{-# HLINT ignore "Use second" #-}
{-# HLINT ignore "Use list comprehension" #-}

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
      valids = do
        toConsider <- numberLines
        let validEdges = removeIngoredNodes pairs toConsider
        let sorted = runTopSort validEdges
        if isInSameOrder toConsider sorted then [midEl toConsider] else []
   in show . sum $ valids

part2 :: [String] -> String
part2 lines =
  let (pairs, numberLines) = parseInput lines
      valids = do
        toConsider <- numberLines
        let validEdges = removeIngoredNodes pairs toConsider
        let sorted = runTopSort validEdges
        if isInSameOrder toConsider sorted then [] else [midEl sorted]
   in show . sum $ valids

isInSameOrder :: (Eq a) => [a] -> [a] -> Bool
isInSameOrder [] [] = True
isInSameOrder (a : as) (b : bs) = a == b && isInSameOrder as bs
isInSameOrder _ _ = False

midEl :: [a] -> a
midEl x = x !! (length x `div` 2)

computeNodeIncomingDegrees :: [(Int, Int)] -> Map Int Int
computeNodeIncomingDegrees edges =
  let initialMap = foldl (\acc (from, to) -> Map.insert from 0 (Map.insert to (0 :: Int) acc)) Map.empty edges
      incomingDegrees = foldl (\acc (from, to) -> Map.adjust (+ (1 :: Int)) to acc) initialMap edges
   in incomingDegrees

removeIngoredNodes :: [(Int, Int)] -> [Int] -> [(Int, Int)]
removeIngoredNodes edges consideredNodes = do
  (from, to) <- edges
  if from `elem` consideredNodes && to `elem` consideredNodes
    then [(from, to)]
    else []

data TopSortStep = TopSortStep
  { edges :: [(Int, Int)],
    partialSort :: [Int],
    incomingDegrees :: Map Int Int
  }
  deriving (Show)

runTopSort :: [(Int, Int)] -> [Int]
runTopSort edx =
  let degs = computeNodeIncomingDegrees edx
      runUntilDone :: TopSortStep -> [Int]
      runUntilDone crtStep =
        case topSortStep crtStep of
          Just nextStep -> runUntilDone nextStep
          Nothing -> reverse $ partialSort crtStep
   in runUntilDone (TopSortStep edx [] degs)

topSortStep :: TopSortStep -> Maybe TopSortStep
topSortStep crtStep = do
  nodeWithZero <- fmap fst $ maybeHead $ filter (\(node, degree) -> degree == 0) (Map.assocs $ incomingDegrees crtStep)
  let (newEdges, nodesToDcrement) =
        foldl
          ( \(newEdges, nodesToDecrease) (from, to) ->
              if from == nodeWithZero
                then (newEdges, to : nodesToDecrease)
                else ((from, to) : newEdges, nodesToDecrease)
          )
          ([], [])
          (edges crtStep)
  let newDegrees = foldl (flip (Map.adjust (\x -> x - 1))) (Map.delete nodeWithZero $ incomingDegrees crtStep) nodesToDcrement
  return $ TopSortStep newEdges (nodeWithZero : partialSort crtStep) newDegrees

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x : _) = Just x

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
