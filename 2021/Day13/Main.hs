import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.List as L

import Data.Set (Set)
import qualified Data.Set as S

main = part2

part2 = do
  lines <- readLines "input.txt"
  let (board, rest) = parseBoardLine emptyBoard lines
  let folds = map parseFold rest
  let result = foldl foldWithCollect [board] folds
  putStrLn . printBoard . head $ result
  return ()

foldWithCollect :: [Board] -> Fold -> [Board]
foldWithCollect acc crt = 
  let r = head acc `fold` crt
  in  r:acc

part1 = do
  lines <- readLines "input.txt"
  let (board, rest) = parseBoardLine emptyBoard lines
  let folds = map parseFold rest
  let firstFold = head folds
  let folded1 = board `fold` firstFold
  _ <- print . length . points $ folded1
  return ()

printBoard :: Board -> String
printBoard board = 
  let lines = do
        y <- [0..(height board - 1)]
        let line = do
              x <- [0..(width board - 1)]
              return $ board `getElementAt` (x, y)
        return line
  in  L.intercalate "\n" lines

getElementAt :: Board -> (Int, Int) -> Char
getElementAt (Board points _ _) key = if key `elem` points then '#' else '.'

foldTopOnBottom :: Int -> (Int, Int) -> (Int, Int)
foldTopOnBottom splittingLine (x, y) = (x, 2 * splittingLine - y)

mirrorTop :: Int -> (Int, Int) -> (Int, Int)
mirrorTop height (x, y) = (x, height - y - 1)

foldBottonOnTop :: Int -> (Int, Int) -> (Int, Int)
foldBottonOnTop splittingLine (x, y) = (x, 2 * splittingLine - y)

translateBottomOnTop :: Int -> (Int, Int) -> (Int, Int)
translateBottomOnTop splittingLine (x, y) = (x, y - splittingLine - 1)

fold :: Board -> Fold -> Board
fold board foldType = case foldType of FoldX _ -> foldX board foldType
                                       FoldY _ -> foldY board foldType

foldY :: Board -> Fold -> Board
foldY board (FoldY splittingLine) = 
  let keepSmaller (_, y) = y < splittingLine
      keepBigger (_, y) = y > splittingLine
      (topHeight, bottomHeight) = (splittingLine, height board - splittingLine - 1)
      (transformTop, transformBottom) = if topHeight >= bottomHeight
        then (id, foldBottonOnTop splittingLine)
        else (mirrorTop splittingLine, translateBottomOnTop splittingLine)
      upperPart = map transformTop $ filter keepSmaller . S.toList . points $ board
      lowerPart = map transformBottom $ filter keepBigger . S.toList . points $ board
      newBoard = foldl addPoint emptyBoard (upperPart <> lowerPart)
      newHeight = if topHeight > bottomHeight then topHeight else bottomHeight
  in  setWidth (setHeight newBoard newHeight) (width board)

foldX :: Board -> Fold -> Board
foldX board (FoldX splittingLine) = 
  let rotated = rotate board
      foldedY = foldY rotated $ FoldY splittingLine
  in  rotate foldedY

-- foldX :: Board -> Fold -> Board
-- foldX board (FoldX splittingLine) = 
--   let keepLeft (x, _) = x < splittingLine
--       keepRight (x, _) = x > splittingLine
--       (leftWidth, rightWidth) = (splittingLine, width board - splittingLine - 1)
--       (transformLeft, transformRight) = if leftWidth >= rightWidth
--         then (id, foldRightOnLeft splittingLine)
--         else (foldLeftOnRight splittingLine, translateRightOnLeft splittingLine)
--       upperPart = map transformTop $ filter keepSmaller . S.toList . points $ board
--       lowerPart = map transformBottom $ filter keepBigger . S.toList . points $ board
--       newBoard = foldl addPoint emptyBoard (upperPart <> lowerPart)
--   in  newBoard


data Fold = FoldX Int | FoldY Int 
  deriving (Show)

parseFold :: String -> Fold
parseFold line = 
  let [a, b] = splitOn "=" line
      ammt = toInt b
  in  if a == "fold along y" then FoldY ammt else FoldX ammt

data Board = Board { 
  points :: Set (Int, Int),
  width :: Int,
  height :: Int
} deriving (Show, Eq)

emptyBoard :: Board
emptyBoard = Board S.empty 0 0

addPoint :: Board -> (Int, Int) -> Board
addPoint board (x, y) = 
  let crtW = width board
      crtH = height board
      newW = if crtW > (x + 1) then crtW else x + 1
      newH = if crtH > (y + 1) then crtH else y + 1
      newPoints = (x, y) `S.insert` points board
  in  Board newPoints newW newH

rotate :: Board -> Board
rotate (Board points width height) = Board (S.map (\(a, b) -> (b, a)) points) height width

setHeight :: Board -> Int -> Board
setHeight (Board points width height) newHeight = Board points width newHeight

setWidth :: Board -> Int -> Board
setWidth (Board points width height) newWidth = Board points newWidth height

parseBoardLine :: Board -> [String] -> (Board, [String])
parseBoardLine board [] = (board, [])
parseBoardLine board ("":tail) = (board, tail)
parseBoardLine board (crtLine:tail) =
  let [x, y] = parseCoords crtLine
      newBoard = board `addPoint` (x, y)
  in  parseBoardLine newBoard tail

parseCoords :: String -> [Int]
parseCoords = map toInt . splitOn ","

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

-- Bug 1 - fold X not working properly
-- Bug 2 - the widths and heights are not correct after folds 
--         I should not discard the empty lines or columns