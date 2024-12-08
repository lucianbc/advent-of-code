import Debug.Trace
import System.Environment
import Text.Regex.PCRE

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
  let allInput = foldr (\crt acc -> acc ++ "\n" ++ crt) "" lines
      myPattern = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"
      allMatches = (allInput =~ myPattern) :: [[String]]
      compute :: [String] -> Int
      compute [match, a, b] = (read a :: Int) * (read b :: Int)
   in show $ sum $ map compute allMatches

part2 :: [String] -> String
part2 lines =
  let allInput = foldl (\acc crt -> acc ++ "\n" ++ crt) "" lines
      myPattern = "(?:(mul)\\(([0-9]{1,3}),([0-9]{1,3})\\))|(do\\(\\))|(don't\\(\\))"
      allMatches = (allInput =~ myPattern) :: [[String]]
      agg2 :: String -> Int -> [[String]] -> Int
      agg2 _ acc [] = acc
      agg2 cmd acc (m : ms) =
        let [match, _, a, b, kDo, kDon] = m
            toAdd = (read a :: Int) * (read b :: Int)
            toExecute
              | match == "do()" = "do"
              | match == "don't()" = "don't"
              | otherwise = "mul"
            toExecute2 = trace (show m ++ " " ++ show acc) toExecute
         in case (cmd, toExecute2) of
              ("do", "do") -> agg2 "do" acc ms
              ("do", "mul") -> agg2 "do" (acc + toAdd) ms
              ("do", "don't") -> agg2 "don't" acc ms
              ("don't", "do") -> agg2 "do" acc ms
              ("don't", "mul") -> agg2 "don't" acc ms
              ("don't", "don't") -> agg2 "don't" acc ms
   in show $ agg2 "do" 0 allMatches

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
