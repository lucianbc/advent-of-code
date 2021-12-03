main = do
  lines <- readLines "2021/Day3/input.txt"
  print $ part2 lines

part1 :: [String] -> Int
part1 lines = 
  let ones = countOnes lines
      (gamma, epsilon) = computeValues ones lines
  in  gamma * epsilon

part2 :: [String] -> Int
part2 lines =
  let parsedLines = map (\s -> map toInt s) lines
      oxyNum = filterStuff (\ones zeroes -> if (ones >= zeroes) then 1 else 0) 0 parsedLines
      co2Num = filterStuff (\ones zeroes -> if (ones >= zeroes) then 0 else 1) 0 parsedLines
  in  (toDecimal oxyNum) * (toDecimal co2Num)

filterStuff :: (Int -> Int -> Int) -> Int -> [[Int]] -> [Int]
filterStuff criteriaFn position candidates = 
  let countOfOnes = sum $ map (\x -> x !! position) candidates
      countOfZero = length candidates - countOfOnes
      criteriaBit = criteriaFn countOfOnes countOfZero
      filtered = filter (\x -> x !! position == criteriaBit) candidates
  in  if (length candidates == 1)
        then (candidates !! 0)
        else filterStuff criteriaFn (position + 1) filtered 

filterByBitPatternRec :: Int -> [[Int]] -> [Int] -> [Int]
filterByBitPatternRec pos numbers pattern =
  let criteria = pattern !! pos
      filtered = filter (\x -> x !! pos == criteria) numbers
  in  if (length numbers == 1) 
        then (numbers !! 0) 
        else filterByBitPatternRec (pos + 1) filtered pattern

computeValues :: [Int] -> [String] -> (Int, Int)
computeValues countOfOnes lines =
  let totalCount = length lines
      gamma = map (\x -> if (x > (totalCount - x)) then 1 else 0) countOfOnes
      epsilon = map (\x -> if (x > (totalCount - x)) then 0 else 1) countOfOnes
  in  (toDecimal gamma, toDecimal epsilon)

toDecimal :: [Int] -> Int
toDecimal binary = foldl (\acc crt -> 2 * acc + crt) 0 binary

countOnes :: [String] -> [Int]
countOnes measures = 
  let initialCounts = take (length $ head measures) $ repeat 0
  in  foldl countMeasure initialCounts measures

countMeasure :: [Int] -> String -> [Int]
countMeasure acc crt = 
  let zipped = zip acc crt
  in  map (\(x, y) -> x + (toInt y)) zipped

toInt :: Char -> Int
toInt '1' = 1
toInt '0' = 0

readLines :: FilePath -> IO [String]
readLines = fmap lines <$> readFile