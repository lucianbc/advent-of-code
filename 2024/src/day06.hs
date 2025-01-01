{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Use uncurry" #-}
{-# HLINT ignore "Use zipWith" #-}

import Data.Array.Base (UArray (UArray))
import Data.Array.Unboxed qualified as A
import Data.List qualified as L
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Debug.Trace (traceShow, traceShowId)
import System.Environment

main = do
  lines <- readInput
  part <- readPart
  let result = case part of
        "part1" -> part1 lines
        "part2" -> part2 lines
        _ -> "Bad part argument. Must be \"part1\" or \"part2\""
  putStrLn $ "Result is \n" ++ result

part1 :: [String] -> String
part1 lines =
  let (t, pos) = parseTable lines
      path = walk t pos
      pathLength = length . L.nub . map (\x -> (line x, col x)) $ path
   in show pathLength

part2 :: [String] -> String
part2 lines =
  let (t, pos) = parseTable lines
      path = L.nub . map (\x -> (line x, col x)) $ walk t pos
      leadingToLoops = do
        (line, col) <- path
        let newTable = t `put` [((line, col), '#')]
        let newWalk = walk newTable pos
        if detectCycle newWalk then [1] else []
      indexes = map fst $ zip [1 ..] leadingToLoops
   in --  in show indexes <- slow, so show progress. I only care of the last element,
      --  which counts the number of configs that lead to a cycle
      show . last $ indexes

detectCycle :: (Ord a) => [a] -> Bool
detectCycle = detectCycleInternal S.empty
  where
    detectCycleInternal :: (Ord a) => Set a -> [a] -> Bool
    detectCycleInternal _ [] = False
    detectCycleInternal seenSoFar (x : xs) =
      (x `S.member` seenSoFar) || detectCycleInternal (x `S.insert` seenSoFar) xs

data Table = Table
  { width :: Int,
    height :: Int,
    table :: UArray (Int, Int) Char
  }
  deriving (Show)

parseTable :: [String] -> (Table, Position)
parseTable lines =
  let w = length . head $ lines
      h = length lines
      charMap = do
        (lineIndex, line) <- zip [0 :: Int ..] lines
        (colIndex, char) <- zip [0 :: Int ..] line
        return ((lineIndex, colIndex), char)
      (markerPos, marker) = head $ do
        (ix, char) <- charMap
        if char `elem` "<>^v" then [(ix, char)] else []
      charMap2 = do
        (ix, char) <- charMap
        return $ if ix == markerPos then (ix, '.') else (ix, char)
      table = Table w h (A.array ((0, 0), (w - 1, h - 1)) charMap2 :: UArray (Int, Int) Char)
      position = Position (fst markerPos) (snd markerPos) marker
   in (table, position)

printTable :: Table -> String
printTable t = unlines [[table t A.! (r, c) | c <- [0 .. width t - 1]] | r <- [0 .. height t - 1]]

data Position = Position
  { line :: Int,
    col :: Int,
    orientation :: Char
  }
  deriving (Show, Eq, Ord)

nextPosition :: Table -> Position -> Maybe Position
nextPosition t crt =
  let nextPos = case orientation crt of
        '^' -> (line crt - 1, col crt)
        '>' -> (line crt, col crt + 1)
        'v' -> (line crt + 1, col crt)
        '<' -> (line crt, col crt - 1)
      nextChar = t `at` nextPos
   in if isInBounds t nextPos
        then
          if isBlocked nextChar
            then Just $ Position (line crt) (col crt) (turn . orientation $ crt)
            else Just $ Position (fst nextPos) (snd nextPos) (orientation crt)
        else Nothing

walk :: Table -> Position -> [Position]
walk table crtPos =
  let nextPos = nextPosition table crtPos
   in case nextPos of
        Just next -> crtPos : walk table next
        Nothing -> [crtPos]

isBlocked = (== '#')

turn '^' = '>'
turn '>' = 'v'
turn 'v' = '<'
turn '<' = '^'

isInBounds :: Table -> (Int, Int) -> Bool
isInBounds t (l, c)
  | l < 0 || c < 0 = False
  | l >= height t || c >= width t = False
  | otherwise = True

put :: Table -> [((Int, Int), Char)] -> Table
put t updates = Table (width t) (height t) (table t A.// updates)

at :: Table -> (Int, Int) -> Char
at t ix = table t A.! ix

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
