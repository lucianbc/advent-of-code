main = do
  inputLines <- readLines "2021/Day1/input.txt"
  print $ part2 inputLines

part1 :: [String] -> Int
part1 inputLines = 
  let inputNumers = parseLines inputLines
  in  countIncreases inputNumers

part2 :: [String] -> Int
part2 inputLines =
  let inputNumers = parseLines inputLines
      denoised = denoise inputNumers
  in  countIncreases denoised

denoise :: [Int] -> [Int]
denoise measures = 
  let triples = zip3 measures (tail measures) (tail $ tail measures)
  in  map (\(a, b, c) -> a + b + c) triples

countIncreases :: [Int] -> Int
countIncreases numbers = 
  let pairs = zip numbers $ tail numbers
  in  count (\(left, right) -> left < right) pairs

count :: (a -> Bool) -> [a] -> Int
count = countRec 0
  where countRec :: Int -> (a -> Bool) -> [a] -> Int
        countRec n pred [] = n
        countRec n pred (head:tail) = 
          countRec (n + if (pred head) then 1 else 0) pred tail

parseLines :: [String] -> [Int]
parseLines = map toInt

toInt :: String -> Int
toInt = read

readLines :: FilePath -> IO [String]
readLines = fmap lines <$> readFile
