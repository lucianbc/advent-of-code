import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

import qualified Data.List as L

import Control.Monad.State.Lazy

import Debug.Trace

bump :: Board -> Board
bump = fmap (fmap (\x -> if (x + 1) == 10 then 1 else x + 1))

bumpStream :: Board -> [Board]
bumpStream crt = crt : (bumpStream . bump $ crt)

expandBoard :: Board -> Board -> Int -> Board
expandBoard seed previous times =
  let column = take (times - 1) $ bumpStream seed
      row = take times $ bumpStream seed
      newRows = composeLines row
  in  composer previous (concat column) <> newRows

boardStream :: Board -> [Board]
boardStream b = b : boardStreamRec (bump b) b 2
  where boardStreamRec :: Board -> Board -> Int -> [Board]
        boardStreamRec seed prev count = 
          let crt = expandBoard seed prev count
          in  crt : boardStreamRec (bump seed) crt (count + 1)

composer :: Board -> Board -> Board
composer [] a = a
composer a [] = a
composer a b = zipWith (<>) a b

composeLines :: [Board] -> Board
composeLines = foldl composer []

main = part1

-- part 2 is very slow with my implementation
part2 = do
  lines <- readLines "input.txt"
  let board = parseBoard lines
  let bs = boardStream board
  let bigBoard = last $ take 5 bs
  print $ findCost bigBoard
  return ()

findCost board = 
  let initialMap = M.fromList [((1, 1), 1)]
      allCosts = findMinCost3 board initialMap [((1, 1), 1)]
      last = M.lookup (width board, height board) allCosts
      first = M.lookup (1, 1) allCosts
      result = do 
        l <- last
        f <- first
        return $ l - f
  in  result

part1 = do
  lines <- readLines "input.txt"
  let board = parseBoard lines
  print $ findCost board
  return ()

printBoard :: Board -> String 
printBoard board = 
  let x = concatMap show <$> board
  in  L.intercalate "\n" x

parseBoard :: [String] -> Board
parseBoard = map (map charToInt)

type Board = [[Int]]

getAt :: Board -> (Int, Int) -> Int
getAt board (line, col) = board !! (line - 1) !! (col - 1)

width :: Board -> Int
width = length . head

height :: Board -> Int
height = length

type CostMemo = Map (Int, Int) Int

inBounds :: Board -> [(Int, Int)] -> [(Int, Int)]
inBounds board = filter (\(line, col) -> line > 0 && line <= height board && col > 0 && col <= width board)

isInBounds :: Board -> (Int, Int) -> Bool
isInBounds board (line, col) = line > 0 && line <= height board && col > 0 && col <= width board

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (line, col) = [(line + 1, col), (line - 1, col), (line, col + 1), (line, col - 1)]

type DNode = ((Int, Int), Int)
type Queue = [DNode]

addNode :: DNode -> Queue -> Queue
addNode node [] = [node]
addNode node queue = 
  if snd node <= snd (head queue) 
    then node : queue
    else head queue : addNode node (tail queue)

getNext :: Queue -> (DNode, Queue)
getNext (x:xs) = (x, xs)

findMinCost3 :: Board -> CostMemo -> Queue -> CostMemo
findMinCost3 board costsSoFar [] = costsSoFar
findMinCost3 board costsSoFar queue = 
  let 
      -- ((crtPos, crtDist), rest) = trace ("elems queue " ++ show (take 3 queue)) (getNext queue)
      ((crtPos, crtDist), rest) = getNext queue
      positionsAround = filter (isInBounds board) (neighbors crtPos)
      distancesAround = (+ crtDist) <$> map (getAt board) positionsAround
      shouldUpdate :: DNode -> Bool
      shouldUpdate (pos, dist) = case M.lookup pos costsSoFar of
        Nothing -> True
        Just c -> c > dist
      toUpdate = filter shouldUpdate $ zip positionsAround distancesAround
      newCosts = foldl (\acc (pos, cost) -> M.insert pos cost acc) costsSoFar toUpdate
      newQueue = foldl (\acc crt -> addNode crt acc) rest toUpdate
  in  findMinCost3 board newCosts newQueue  

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
