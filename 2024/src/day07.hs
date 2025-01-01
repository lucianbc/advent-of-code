{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

{-# HLINT ignore "Use list comprehension" #-}

import Data.Text qualified as T
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
  let inputs = map parseLine lines
      oks = do
        (expected, nums) <- inputs
        let evaluations = evaluateX [(+), (*)] nums
        if expected `elem` evaluations
          then [expected]
          else []
   in show $ sum oks

part2 :: [String] -> String
part2 lines =
  let inputs = map parseLine lines
      oks = do
        (expected, nums) <- inputs
        let evaluations = evaluateX [(+), (*), Main.concat] nums
        if expected `elem` evaluations
          then [expected]
          else []
   in show $ sum oks

parseLine :: String -> (Int, [Int])
parseLine s =
  let [n, rest] = T.splitOn (T.pack ":") (T.pack s)
      operands :: [Int]
      operands = map (read . T.unpack) (T.words rest)
   in (read (T.unpack n) :: Int, operands)

evaluateX :: [Int -> Int -> Int] -> [Int] -> [Int]
evaluateX _ [] = []
evaluateX ops (x : xs) = evaluateInternal xs [x]
  where
    evaluateInternal :: [Int] -> [Int] -> [Int]
    evaluateInternal [] evals = evals
    evaluateInternal (x : xs) evals =
      let opsApplied = do
            op <- ops
            map (`op` x) evals
       in evaluateInternal xs opsApplied

concat :: Int -> Int -> Int
concat a b = read $ show a ++ show b

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
