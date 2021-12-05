import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as M

type Point = (Int, Int)
type Line = (Point, Point)
type Board = Map (Int, Int) Int

main = part1

part1 = do
  fileLines <- readLines "2021/Day5/input.txt"
  let lines = filterVerticalHorizontal $ map parseLine fileLines
  let intersections = countLineIntersections lines
  print intersections

part2 = do
  fileLines <- readLines "2021/Day5/input.txt"
  let lines = map parseLine fileLines
  let intersections = countLineIntersections lines
  print intersections

countLineIntersections :: [Line] -> Int
countLineIntersections lines = 
  let allPoints = concat $ map lineToPoints lines
      finalBoard = foldl incrementOnMap M.empty allPoints
      intersections = M.foldl
        (\acc crt -> acc + if (crt >= 2) then 1 else 0)
        0 finalBoard
  in  intersections

incrementOnMap :: Board -> Point -> Board
incrementOnMap board point =
  let maybeCount = M.lookup point board
      ct = case maybeCount of Just n -> n
                              Nothing -> 0
  in  M.insert point (ct + 1) board

lineToPoints :: Line -> [Point]
lineToPoints line 
  | x1 line == x2 line = verticalPoints line
  | y1 line == y2 line = horizontalPoints line
  | otherwise = diagonalPoints line
  
verticalPoints :: Line -> [Point]
verticalPoints line =
  let minY = min (y1 line) (y2 line)
      maxY = max (y1 line) (y2 line)
      x = x1 line
  in  map (\y -> (x,y)) [minY..maxY]

horizontalPoints :: Line -> [Point]
horizontalPoints line = 
  let
    minX = min (x1 line) (x2 line)
    maxX = max (x1 line) (x2 line)
    y = y1 line
  in map (\x -> (x,y)) [minX..maxX]

diagonalPoints :: Line -> [Point]
diagonalPoints line =
  let xIncrement = if (x1 line < x2 line) then 1 else -1
      yIncrement = if (y1 line < y2 line) then 1 else -1
      steps = abs $ (x1 line) - (x2 line)
      ((x, y), _) = line
      mapper :: Int -> (Int, Int)
      mapper magnitude = (x + magnitude * xIncrement, y + magnitude * yIncrement)
  in  map mapper [0..steps]

x1 = fst . fst
y1 = snd . fst
x2 = fst . snd
y2 = snd . snd

filterVerticalHorizontal :: [Line] -> [Line]
filterVerticalHorizontal =
  let toKeep ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2
  in  filter toKeep

parseLine :: String -> Line
parseLine line = 
  let pointsStr = splitOn " -> " line
      [[xs1, ys1], [xs2, ys2]] = map (splitOn ",") pointsStr
  in  ((toInt xs1, toInt ys1), (toInt xs2, toInt ys2))

splitOn :: String -> String -> [String]
splitOn delim = map T.unpack . T.splitOn (T.pack delim) . T.pack

toInt :: String -> Int
toInt = read

readLines :: FilePath -> IO [String]
readLines = fmap lines <$> readFile

-- initial attempts to review later
type Board2 = [[Int]]

increment :: Board2 -> Point -> Board2
increment board (x, y) =
  let indexedRows = zip [0,1..] board :: [(Int, [Int])]
      indexCells :: [Int] -> [(Int, Int)]
      indexCells cells = zip [0,1..] cells
      incrementCell :: (Int, Int) -> Int
      incrementCell (index, cell) = if (index == x) then cell + 1 else cell
      incrementOnRow :: (Int, [Int]) -> [Int]
      incrementOnRow (index, row) = 
        if (index == y) 
          then map incrementCell (indexCells row)
          else row
  in  map incrementOnRow indexedRows

part11 = do
  fileLines <- readLines "2021/Day5/input.txt"
  let lines = map parseLine fileLines
  --let intersections = countIntersectionsByEq $ filterVerticalHorizontal lines
  --let intersections = countIntersectionsOnBoard $ filterVerticalHorizontal lines
  intersections <- countIntersectionsOnBoardIO $ filterVerticalHorizontal lines
  _ <- print intersections
  return ()

countIntersectionsOnBoardIO :: [Line] -> IO Int
countIntersectionsOnBoardIO lines = do
  let board = createBoard lines
  _ <- print "done board"
  let allPoints = concat $ map lineToPoints $ filterVerticalHorizontal lines
  _ <- print "done allpoints"
  let finalBoard = foldl increment board allPoints
  let intersections = foldl (\acc crt -> acc + if (crt >= 2) then 1 else 0) 0 $ concat finalBoard
  return intersections

countIntersectionsOnBoard :: [Line] -> Int
countIntersectionsOnBoard lines = 
  let board = createBoard lines
      allPoints = concat $ map lineToPoints $ filterVerticalHorizontal lines
      finalBoard = foldl increment board allPoints
      intersections = foldl (\acc crt -> acc + if (crt >= 2) then 1 else 0) 0 $ concat finalBoard
  in  intersections

countIntersectionsByEq :: [Line] -> Int
countIntersectionsByEq [] = 0
countIntersectionsByEq (line:rest) = 
  let restIntersections = countIntersectionsByEq rest
      foldFn :: Int -> Line -> Int
      foldFn acc crt = acc + (countOverlaps line crt)
      crtIntersections = foldl foldFn 0 rest
  in  crtIntersections + restIntersections


-- assume lines are either vertical or horizontal
countOverlaps :: Line -> Line -> Int
countOverlaps l1 l2 =
  if linesIntersect l1 l2
    then 1
    else 
      if (x1 l1 == x2 l1 && x1 l2 == x2 l2 && x1 l1 == x1 l2)
        then overlap (y1 l1) (y2 l1) (y1 l2) (y2 l2)
        else
          if (y1 l1 == y2 l1 && y1 l2 == y2 l2 && y1 l1 == y1 l2)
            then overlap (x1 l1) (x2 l1) (x1 l2) (x2 l2)
          else 0

overlap :: Int -> Int -> Int -> Int -> Int
overlap a b c d =
  let min1 = min a b
      max1 = max a b
      min2 = min c d
      max2 = max c d
      biggerMin = max min1 min2
      smallerMax = min max1 max2
  in  if smallerMax < biggerMin then 0 else smallerMax - biggerMin + 1

-- https://stackoverflow.com/questions/3838329/how-can-i-check-if-two-segments-intersect
linesIntersect :: Line -> Line -> Bool
linesIntersect (a, b) (c, d) = 
  let ccw :: Point -> Point -> Point -> Bool
      ccw (ax, ay) (bx, by) (cx, cy) = (cy - ay) * (bx - ax) > (by - ay) * (cx - ax)
  in (ccw a c d) /= (ccw b c d) && (ccw a b c) /= (ccw a b d) 

createBoard :: [Line] -> Board2
createBoard lines = 
  let width = maxX lines + 1
      height = maxY lines + 1
  in  take height (repeat (take width $ repeat 0))

allX = (>>= (\line -> [x1 line, x2 line]))
allY = (>>= (\line -> [y1 line, y2 line]))

maxList = foldl max 0

maxX lines = maxList $ allX lines
maxY lines = maxList $ allY lines


