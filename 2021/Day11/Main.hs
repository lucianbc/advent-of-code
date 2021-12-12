import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.List as L

import Data.Set (Set)
import qualified Data.Set as S

import Control.Monad.State.Lazy

import Data.Maybe

main = part2

part2 = do
  board <- readBoard
  let flashes = zip [1,2..] (streamFlashCounts board)
  let firstAllFlash = L.find (\a -> snd a == 100) flashes
  print firstAllFlash
  
part1 = do
  board <- readBoard
  let flashes = take 100 $ streamFlashCounts board
  _ <- print $ sum flashes
  return ()

type Board = Map (Int, Int) Int

streamFlashCounts :: Board -> [Int]
streamFlashCounts board = 
  let (nextBoard, flashed) = nextGeneration board
  in flashed : streamFlashCounts nextBoard

nextGeneration :: Board -> (Board, Int)
nextGeneration board = 
  let incMap = M.map (+ 1) board
  in  evalState (countFlashes incMap) S.empty

incrementIfPresent :: Board -> (Int, Int) -> Board
incrementIfPresent board key = 
  let newBoardMaybe = do
        value <- M.lookup key board
        return $ M.insert key (value + 1) board
  in  Data.Maybe.fromMaybe board newBoardMaybe

countFlashes :: Board -> State (Set (Int, Int)) (Board, Int)
countFlashes board = do
  previouslyFlashed <- get
  let allBoardElems = M.toList board
  let nextToFlash = do
        (key, value) <- allBoardElems
        [key | value > 9 && not (key `S.member` previouslyFlashed)]
  let adjList = concatMap indices nextToFlash
  let adjacents = adjList
  if null nextToFlash
    then return (resetFlashed board, length previouslyFlashed) 
    else do 
      put $ (S.union . S.fromList) nextToFlash previouslyFlashed
      countFlashes (foldl incrementIfPresent board adjacents)

resetFlashed :: Board -> Board
resetFlashed = M.map (\x -> if x > 9 then 0 else x)

increments = [(-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1)]
indices :: (Int, Int) -> [(Int, Int)]
indices (line, col) = map (\(a, b) -> (line + a, col +b)) increments

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
