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
  let input = parseInput lines
   in show $ length $ filter isSafe input

part2 :: [String] -> String
part2 lines =
  let input = parseInput lines
   in show . length $ filter isSafe2 input

isSafe2 :: [Int] -> Bool
isSafe2 nums =
  let allVariants = generateAllIgnoringOne nums
   in any isSafe allVariants

generateAllIgnoringOne :: [Int] -> [[Int]]
generateAllIgnoringOne nums = do
  pos <- [0 .. (length nums)]
  return $ removeAt pos nums

removeAt :: Int -> [Int] -> [Int]
removeAt position nums =
  let indexed = zip [0 ..] nums
      filtered = filter (\(index, num) -> index /= position) indexed
   in map snd filtered

isSafe :: [Int] -> Bool
isSafe xs = isSafeRec ((xs !! 1) - head xs) xs

isSafeRec :: Int -> [Int] -> Bool
isSafeRec signature [] = True
isSafeRec signature [a, b] =
  -- if signature is smaller than 0, then I expect a to be greater than b
  let (left, right) = if signature < 0 then (a, b) else (b, a)
      diff = left - right
   in diff >= 1 && diff <= 3
isSafeRec signature (a : b : xs) = isSafeRec signature [a, b] && isSafeRec signature (b : xs)

parseInput :: [String] -> [[Int]]
parseInput lines =
  let parseLine = map (read :: String -> Int) . words
   in map parseLine lines

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
