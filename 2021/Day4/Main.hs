import qualified Data.Text as T

main = part2

runAllBingo :: [Int] -> [Table] -> Maybe WinnerCombo -> Maybe WinnerCombo
runAllBingo [] _ prevWinner = prevWinner
runAllBingo (crtNum:rest) tables prevWinner =
  let markedTables = map (mark crtNum) tables
      (winnerIndex, possibleWinner) = findWinner markedTables
      (nextWinner, nextTables) = 
        case possibleWinner of 
          Just winner -> (Just (winner, crtNum), removeAt winnerIndex markedTables)
          Nothing -> (prevWinner, markedTables)
  in runAllBingo rest nextTables nextWinner


part2 = do
  lines <- readLines "2021/Day4/input.txt"
  let numbers = parseNumbers $ head lines
  let tables = parseTables $ (tail $ tail lines)
  let result = runAllBingo numbers tables Nothing
  _ <- print result
  let score = case result of
                Just (winner, lastNum) -> computeScore lastNum winner   
                Nothing -> -1
  print score

showTables :: [Table] -> [[Int]]
showTables tables = 
  let showTable table = map (\x -> fst x) (head table)
  in  map showTable tables

part1 = do
  lines <- readLines "2021/Day4/example.txt"
  let numbers = parseNumbers $ head lines
  let tables = parseTables $ (tail $ tail lines)
  let (winner, lastNum) = runBingo numbers tables
  _ <- print winner
  _ <- print lastNum
  _ <- print $ computeScore lastNum winner
  return ()

type TableCell = (Int, Bool)
type Table = [[TableCell]]
type WinnerCombo = (Table, Int)

removeAt :: Int -> [a] -> [a]
removeAt pos elems = removeCount 0 pos elems
  where removeCount :: Int -> Int -> [a] -> [a]
        removeCount _ _ [] = []
        removeCount ct pos (x:xs) = if (ct == pos) then xs else x:(removeCount (ct + 1) pos xs)

runBingo :: [Int] -> [Table] -> (Table, Int)
runBingo (crtNum:rest) tables = 
  let markedTables = map (mark crtNum) tables  
      (_, possibleWinner) = findWinner markedTables
  in  case possibleWinner of Just winner -> (winner, crtNum)
                             Nothing -> runBingo rest markedTables

computeScore :: Int -> Table -> Int
computeScore lastNum table = 
  let addToSum = (\acc (crt, marked) -> if (not marked) then acc + crt else acc)
      allNums = concat table
      sumOfMarked = foldl addToSum 0 allNums
  in lastNum * sumOfMarked

findWinner :: [Table] -> (Int, Maybe Table)
findWinner tables = 
  let findWinnerRec :: [Table] -> Int -> (Int, Maybe Table)
      findWinnerRec [] ct = (-1, Nothing)
      findWinnerRec (crt:rest) ct = if (isWinner crt) then (ct, Just crt) else findWinnerRec rest (ct + 1)
  in  findWinnerRec tables 0

findWinners :: [Table] -> [(Int, Table)]
findWinners tables = 
  let findWinnerRec :: [Table] -> Int -> [(Int, Table)]
      findWinnerRec [] ct = []
      findWinnerRec (crt:rest) ct = 
        let restW = findWinnerRec rest (ct + 1)
        in  if (isWinner crt) then (ct, crt):restW else restW
  in  findWinnerRec tables 0

isWinner :: Table -> Bool
isWinner table = 
  let checkForAnyMarked = 
        foldl (\acc crt -> acc || isAllMarked crt) False
      hasAnyNegativeRow = checkForAnyMarked table
      hasAnyNegativeCol = checkForAnyMarked $ transpose table
  in  hasAnyNegativeRow || hasAnyNegativeCol

isAllMarked :: [TableCell] -> Bool
isAllMarked = foldl (\acc (_, crt) -> acc && crt) True

isAllNegative :: [Int] -> Bool
isAllNegative elems = foldl (\acc crt -> acc && (crt < 0)) True elems

transpose :: Table -> Table
transpose table = 
  let heads = map head table
      tails = map tail table
  in  if (head table == []) 
        then []
        else heads:(transpose tails)

markOnLine :: Int -> [TableCell] -> [TableCell]
markOnLine toMark elems = do
  crt <- elems
  let crtVal = fst crt
  return $ if (crtVal == toMark) then (crtVal, True) else crt

mark :: Int -> Table -> Table
mark value table = map (markOnLine value) table

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


-- has a bug - if 0 is in the winning combination, it does not work
-- fixed with the pair with boolean