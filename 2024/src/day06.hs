{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

import System.Environment
import Text.XHtml (height)

main = do
  lines <- readInput
  part <- readPart
  let result = case part of
        "part1" -> part1 lines
        "part2" -> part2 lines
        _ -> "Bad part argument. Must be \"part1\" or \"part2\""
  putStrLn $ "Result is " ++ result

part1 :: [String] -> String
part1 lines = "todo"

part2 :: [String] -> String
part2 lines = "todo"

data Dirrection = Up | Down | DLeft | DRight

data Slot = Empty | Visited | Current Dirrection | Obstacle

type Table = [[Slot]]

width :: Table -> Int
width (x : _) = length x

height :: Table -> Int
height = length

current :: Table -> (Int, Int)
current t = currentInternal 0 t
  where
    currentInternal :: Int -> Table -> (Int, Int)
    currentInternal currentLineIndex (line : ls) = (0, 0)

instance Show Dirrection where
  show Up = "^"
  show Down = "v"
  show DLeft = "<"
  show DRight = ">"

instance Show Slot where
  show Empty = "."
  show Visited = "X"
  show (Current x) = show x
  show Obstacle = "#"

instance Read Slot where
  readsPrec :: Int -> ReadS Slot
  readsPrec _ (x : xs) = case x of
    '.' -> [(Empty, xs)]
    'X' -> [(Visited, xs)]
    '#' -> [(Obstacle, xs)]
    '^' -> [(Current Up, xs)]
    'v' -> [(Current Down, xs)]
    '<' -> [(Current DLeft, xs)]
    '>' -> [(Current DRight, xs)]
    _ -> []
  readsPrec _ [] = []

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
