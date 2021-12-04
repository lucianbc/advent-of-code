import qualified Data.Text as T

main = part2

type TableCell = (Int, Bool)
type Table = [[TableCell]]
type WinnerCombo = (Table, Int)

part2 = do
  (numbers, tables) <- readInput
  let allCombos = runBingo numbers tables
  let result = last allCombos
  let (winner, lastNum) = result
  let score = computeScore lastNum winner
  print score

part1 = do
  (numbers, tables) <- readInput
  let (winner, lastNum) = head $ runBingo numbers tables
  let score = computeScore lastNum winner
  print score

runBingo :: [Int] -> [Table] -> [WinnerCombo]
runBingo [] _ = []
runBingo _ [] = []
runBingo (crtNum:rest) tables = 
  let marked = map (mark crtNum) tables
      (winners, loosers) = splitWinners marked
      winnersCombo = map (\x -> (x, crtNum)) winners
  in  winnersCombo <> (runBingo rest loosers)

splitWinners :: [Table] -> ([Table], [Table])
splitWinners [] = ([], [])
splitWinners (t:tables) = 
  let (winners, loosers) = splitWinners tables
      isCrtWinner = isWinner t
  in  if isCrtWinner then (t:winners, loosers) else (winners, t:loosers)

computeScore :: Int -> Table -> Int
computeScore lastNum table = 
  let addToSum = (\acc (crt, marked) -> if (not marked) then acc + crt else acc)
      allNums = concat table
      sumOfMarked = foldl addToSum 0 allNums
  in lastNum * sumOfMarked

isWinner :: Table -> Bool
isWinner table = 
  let isAllMarked = foldl (\acc (_, crt) -> acc && crt) True
      checkForAnyMarked = 
        foldl (\acc crt -> acc || isAllMarked crt) False
      hasAnyNegativeRow = checkForAnyMarked table
      hasAnyNegativeCol = checkForAnyMarked $ transpose table
  in  hasAnyNegativeRow || hasAnyNegativeCol

transpose :: Table -> Table
transpose table = 
  let heads = map head table
      tails = map tail table
  in  if (head table == []) 
        then []
        else heads:(transpose tails)

mark :: Int -> Table -> Table
mark value = map (markOnLine value)

markOnLine :: Int -> [TableCell] -> [TableCell]
markOnLine toMark elems = do
  crt <- elems
  let crtVal = fst crt
  return $ if (crtVal == toMark) then (crtVal, True) else crt

readInput :: IO ([Int], [Table])
readInput = do
  lines <- readLines "2021/Day4/input.txt"
  let numbers = parseNumbers $ head lines
  let tables = parseTables $ (tail $ tail lines)
  return (numbers, tables)

parseTables :: [String] -> [Table]
parseTables [] = [[]]
parseTables (crtLine:restLines) =
  let (crtTable:restTables) = parseTables restLines
  in  if (crtLine == "") 
        then []:(crtTable:restTables)
        else ((map toCell $ words crtLine):crtTable):restTables

parseNumbers :: String -> [Int]
parseNumbers = map toInt . splitOn ","

splitOn :: String -> String -> [String]
splitOn delim = map T.unpack . T.splitOn (T.pack delim) . T.pack

toCell :: String -> TableCell
toCell str = (toInt str, False)

toInt :: String -> Int
toInt = read

readLines :: FilePath -> IO [String]
readLines = fmap lines <$> readFile