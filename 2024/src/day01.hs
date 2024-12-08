import Data.List (sort, transpose)
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
  let (col1, col2) = parseInput lines
      diffs = zipWith (\a b -> abs (a - b)) col1 col2
      result = sum diffs
   in show result

part2 :: [String] -> String
part2 lines =
  let (col1, col2) = parseInput lines
      similarities = do
        current <- col1
        return $ length $ filter (== current) col2
      result = sum $ zipWith (*) col1 similarities
   in show result

parseInput :: [String] -> ([Int], [Int])
parseInput lines =
  let parseLine = map (read :: String -> Int) . words
      columns = transpose $ map parseLine lines
      sortedColumns = map sort columns
      col1 = head sortedColumns
      col2 = sortedColumns !! 1
   in (col1, col2)

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
