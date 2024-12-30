{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

import Debug.Trace (traceShowId)
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
  let initial = parseTable lines
      allTables = generateTables initial
      (Table endConfig) = last allTables
      count = sum $ do
        l <- endConfig
        char <- l
        case char of
          Visited -> [1]
          _ -> []
   in show count

part2 :: [String] -> String
part2 lines = "todo"

generateTables :: Table -> [Table]
generateTables t =
  case nextTable t of
    Nothing -> [t]
    Just next -> t : generateTables next

nextTable :: Table -> Maybe Table
nextTable table = do
  (crtPos@(crtLine, crtCol), crtSlot) <- current table
  let nextPos = case crtSlot of
        Current Up -> (crtLine - 1, crtCol)
        Current Down -> (crtLine + 1, crtCol)
        Current DLeft -> (crtLine, crtCol - 1)
        Current DRight -> (crtLine, crtCol + 1)
  let nextTable
        | isOutsideBounds nextPos table = put crtPos Visited table
        | checkCanVisit nextPos table = put nextPos crtSlot . put crtPos Visited $ table
        | otherwise = put crtPos (turn crtSlot) table
  return nextTable

turn (Current Up) = Current DRight
turn (Current DRight) = Current Down
turn (Current Down) = Current DLeft
turn (Current DLeft) = Current Up

isOutsideBounds (line, col) table
  | line < 0 || col < 0 = True
  | line >= height table || col >= width table = True
  | otherwise = False

checkCanVisit nextPos table = case elemAt table nextPos of
  Obstacle -> False
  _ -> True

data Direction = Up | Down | DLeft | DRight

data Slot = Empty | Visited | Current Direction | Obstacle

newtype Table = Table [[Slot]]

instance Show Table where
  show (Table lines) = unlines (map (map (head . show)) lines)

elemAt :: Table -> (Int, Int) -> Slot
elemAt (Table ls) (line, col) = ls !! line !! col

put :: (Int, Int) -> Slot -> Table -> Table
put (line, col) slot (Table inTable) =
  let (prevLines, myLine : tailLines) = splitAt line inTable
      (prevSlots, _ : tailSlots) = splitAt col myLine
      newLine = prevSlots ++ (slot : tailSlots)
   in Table (prevLines ++ (newLine : tailLines))

width :: Table -> Int
width (Table (x : _)) = length x

height :: Table -> Int
height (Table x) = length x

current :: Table -> Maybe ((Int, Int), Slot)
current (Table ls) = currentInternal 0 ls
  where
    findCurrentOnLine :: Int -> [Slot] -> Maybe (Int, Slot)
    findCurrentOnLine _ [] = Nothing
    findCurrentOnLine i (crtSlot@(Current _) : _) = Just (i, crtSlot)
    findCurrentOnLine i (_ : xs) = findCurrentOnLine (i + 1) xs
    currentInternal :: Int -> [[Slot]] -> Maybe ((Int, Int), Slot)
    currentInternal _ [] = Nothing
    currentInternal currentLineIndex (line : ls) = case findCurrentOnLine 0 line of
      Just (i, crtSlot) -> Just ((currentLineIndex, i), crtSlot)
      Nothing -> currentInternal (currentLineIndex + 1) ls

instance Show Direction where
  show Up = "^"
  show Down = "v"
  show DLeft = "<"
  show DRight = ">"

instance Show Slot where
  show Empty = "."
  show Visited = "X"
  show (Current x) = show x
  show Obstacle = "#"

parseTable :: [String] -> Table
parseTable = Table . map (map readSlot)

readSlot :: Char -> Slot
readSlot x = case x of
  '.' -> Empty
  'X' -> Visited
  '#' -> Obstacle
  '^' -> Current Up
  'v' -> Current Down
  '<' -> Current DLeft
  '>' -> Current DRight
  _ -> error ("Char " ++ show x ++ " not matched")

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
