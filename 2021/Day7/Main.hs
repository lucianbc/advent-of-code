import qualified Data.Text as T

main = part1
 
part2 = do
  nums <- parseNums
  let minCost = findMinCost nums cost2
  _ <- print minCost
  return ()

part1 = do
  nums <- parseNums
  let minCost = findMinCost nums cost
  _ <- print minCost
  return ()

findMinCost :: [Int] -> ([Int] -> Int -> Int) -> Int
findMinCost nums costFn =
  let (min, max) = minMax nums
      costs = map (costFn nums) [min..max]
      (minCost, _) = minMax costs
  in  minCost

cost :: [Int] -> Int -> Int
cost nums x = sum $ map (\p -> abs $ x - p) nums

cost2 :: [Int] -> Int -> Int
cost2 nums x = 
  let fuelDistance a b = 
        let n = abs $ a - b
        in  n * (n + 1) `div` 2
  in  sum $ map (fuelDistance x) nums

minMax :: [Int] -> (Int, Int)
minMax (a:[]) = (a, a)
minMax (x:rest) = 
  let (min1, max1) = minMax rest
  in  (min min1 x, max max1 x)

parseNums = do
  lines <- readLines "input.txt"
  let line = head lines
  return $ map toInt $ splitOn "," line

splitOn :: String -> String -> [String]
splitOn delim = map T.unpack . T.splitOn (T.pack delim) . T.pack

toInt :: String -> Int
toInt = read

readLines :: FilePath -> IO [String]
readLines = fmap lines <$> readFile
