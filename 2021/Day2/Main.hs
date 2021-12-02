main = do
  inputLines <- readLines "2021/Day2/example.txt"
  print $ part1 inputLines

part1 :: [String] -> Int
part1 lines = 
  let route = map parseLine lines
      (x, depth) = foldl computeNextState (0, 0) route
  in  x * depth

part2 :: [String] -> Int
part2 lines = 
  let route = map parseLine lines
      (lenght, depth, _) = foldl computeNextStep2 (0, 0, 0) route
  in lenght * depth

data Direction = Forward | Up | Down
  deriving Show

parseLine :: String -> (Direction, Int)
parseLine line = 
  let (dirStr:numStr:[]) = words line
  in (parseDirection dirStr, read numStr)
      
parseDirection :: String -> Direction
parseDirection "forward" = Forward
parseDirection "down" = Down
parseDirection "up" = Up

computeNextState :: (Int, Int) -> (Direction, Int) -> (Int, Int)
computeNextState (x, depth) (Forward, ammt) = (x + ammt, depth)
computeNextState (x, depth) (Up, ammt) = (x, depth - ammt)
computeNextState (x, depth) (Down, ammt) = (x, depth + ammt)

computeNextStep2 :: (Int, Int, Int) -> (Direction, Int) -> (Int, Int, Int)
computeNextStep2 (vertical, depth, aim) (Forward, x) = 
  (vertical + x, depth + (x * aim), aim)
computeNextStep2 (vertical, depth, aim) (Up, x) = 
  (vertical, depth, aim - x)
computeNextStep2 (vertical, depth, aim) (Down, x) = 
  (vertical, depth, aim + x)

readLines :: FilePath -> IO [String]
readLines = fmap lines <$> readFile