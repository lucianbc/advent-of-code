import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.List as L

main = part2

part2 = do
  board <- readBoard
  let lowPoints = findLowPoints board
  let basins = map (expandBasin board M.empty) lowPoints
  let sortedSizes = L.sortOn (\x -> -x) $ map M.size basins
  let product = foldl (*) 1 $ take 3 sortedSizes
  _ <- print product
  return ()

part1 = do
  board <- readBoard
  let lowPoints = findLowPoints board
  let lows = map (+ 1) $ flatten $ map (`M.lookup` board) lowPoints
  _ <- print $ sum lows
  return ()

type Board = Map (Int, Int) Int

expandBasin :: Board -> Board -> (Int, Int) -> Board
expandBasin board crtBasin crtPosition = 
  let newBasin = M.insert crtPosition 1 crtBasin
      crtValue = (M.!) board crtPosition
      isValidPosition :: (Int, Int) -> Bool
      isValidPosition (line, col) = 
        let boardValue = M.lookup (line, col) board
            boardValueValid = case boardValue of Just v -> v /= 9 && v > crtValue
                                                 Nothing -> False
            notInBasin = M.notMember (line, col) newBasin
        in  boardValueValid && notInBasin
      indicesAround = filter isValidPosition $ indices crtPosition
      result = foldl (expandBasin board) newBasin indicesAround
  in  result

indices :: (Int, Int) -> [(Int, Int)]
indices (line, col) = [(line + 1, col), (line - 1, col), (line, col + 1), (line, col - 1)]

flatten :: [Maybe a] -> [a]
flatten maybes = do
  m <- maybes
  case m of Just x -> [x]
            Nothing -> []

findLowPoints :: Board -> [(Int, Int)]
findLowPoints board = do
  (key, crt) <- M.toList board
  let cellsAround = flatten $ map (`M.lookup` board) $ indices key
  let lowestNeighbor = L.minimum cellsAround
  ([key | lowestNeighbor > crt])

readBoard = do
  lines <- readLines "input.txt"
  let numLines = map charToInt <$> lines
  return $ parseMap numLines
  
parseMap :: [[Int]] -> Map (Int, Int) Int
parseMap lines = M.fromList $ do
  (lineNum, line) <- zip [0,1..] lines
  (colNum, cell) <- zip [0,1..] line
  return ((lineNum, colNum), cell)

splitOn :: String -> String -> [String]
splitOn delim = map T.unpack . T.splitOn (T.pack delim) . T.pack

charToString :: Char -> String
charToString x = [x]

charToInt :: Char -> Int
charToInt = toInt . charToString

toInt :: String -> Int
toInt = read

readLines :: FilePath -> IO [String]
readLines = fmap lines <$> readFile
