import qualified Data.Text as T

main = part1

part1 = do
  lines <- readLines "2021/Day7/example.txt"
  let line = head lines
  let nums = map toInt $ splitOn "," line
  --let distances = findDistances nums
  --let minDistance = findMinDistance distances
  let minDistance = findDistanceEq nums
  print minDistance
  
findDistanceEq :: [Int] -> (Int, Int)
findDistanceEq p =
  let sp = fromIntegral $ sum p
      n = fromIntegral $ length p
      x = sp / n
      f = \x -> sum $ map (\pi -> abs $ pi - x) p
      variants = [floor x, ceiling x]
      [a, b] = map (computeDistance p) variants
  in  (a, b)


findMinDistance :: [(Int, Int)] -> (Int, Int)
findMinDistance dists = 
  foldl 
    (\acc crt -> if (snd acc < snd crt) then acc else crt)
    (head dists)
    dists

findDistances :: [Int] -> [(Int, Int)]
findDistances nums =
  let (a, b) = minMax nums
      range = [a..b]
      result = map (\x -> (x, computeDistance nums x)) range
  in  result  

computeDistance :: [Int] -> Int -> Int
computeDistance nums reference =
  foldl (\acc crt -> abs (reference - crt) + acc) 0 nums

minMax ::[Int] -> (Int, Int)
minMax (a:[]) = (a, a)
minMax (x:rest) = 
  let (min1, max1) = minMax rest
  in  (min min1 x, max max1 x)

splitOn :: String -> String -> [String]
splitOn delim = map T.unpack . T.splitOn (T.pack delim) . T.pack

toInt :: String -> Int
toInt = read

readLines :: FilePath -> IO [String]
readLines = fmap lines <$> readFile
