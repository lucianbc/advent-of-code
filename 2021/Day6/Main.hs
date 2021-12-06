import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as M

main = solveForDays 256

solveForDays days = do
  input <- parseInput
  print $ countAfterDaysMemo days input

type Memo = Map (Int, Int) Integer

countAfterDaysMemo :: Int -> [Int] -> Integer
countAfterDaysMemo days allFish = 
  let foldFn :: (Memo, Integer) -> Int -> (Memo, Integer)
      foldFn (prevMemo, prevCount) crtFish =
        let (newMemo, crtCount) = fishCountMemo prevMemo crtFish days
        in  (newMemo, prevCount + crtCount)
      (_, result) = foldl foldFn (M.empty, 0) allFish
  in  result

fishCountMemo :: Memo -> Int -> Int -> (Memo, Integer)
fishCountMemo memo internalClock afterGenerations =
  let key = (internalClock, afterGenerations)
      maybeResult = M.lookup key memo

      insert value = M.insert key value memo
      
      result = case maybeResult of  
        Just count -> (memo, count)
        Nothing 
          | afterGenerations <= 0 -> (insert 1, 1)
          | internalClock == 0 -> 
            let (memo1, result1) = fishCountMemo memo 6 (afterGenerations - 1) 
                (memo2, result2) = fishCountMemo memo1 8 (afterGenerations - 1)
                sumcount = result1 + result2
            in  (M.insert key sumcount memo2, sumcount)
          | otherwise -> fishCountMemo memo 0 (afterGenerations - internalClock)
  in  result

parseInput = do
  lines <- readLines "2021/Day6/input.txt"
  return $ (map toInt) . (splitOn ",") . head $ lines

splitOn :: String -> String -> [String]
splitOn delim = map T.unpack . T.splitOn (T.pack delim) . T.pack

toInt :: String -> Int
toInt = read

readLines :: FilePath -> IO [String]
readLines = fmap lines <$> readFile

-----------------------------------------------

part1 = do
  input <- parseInput
  let days = 80
  print $ countAfterDays days input

part2 = do
  input <- parseInput
  let days = 256
  print $ countAfterDaysMemo days input

countAfterDays2 :: Int -> [Int] -> Integer
countAfterDays2 days allFish = sum . map ((flip fishCount) days) $ allFish

fishCount :: Int -> Int -> Integer
fishCount internalClock afterGenerations 
  | afterGenerations <= 0 = 1
  | internalClock == 0 = fishCount 6 (afterGenerations - 1) + fishCount 8 (afterGenerations - 1)
  | otherwise = fishCount 0 (afterGenerations - internalClock)

countAfterDays :: Int -> [Int] -> Int
countAfterDays numOfDays =
  length . last . (take numOfDays) . simulate

simulate :: [Int] -> [[Int]]
simulate crtGen = 
  let next = (nextGen crtGen) 
  in  next : (simulate next)

nextGen :: [Int] -> [Int]
nextGen crtNums = 
  let mapper :: ([Int], [Int]) -> Int -> ([Int], [Int])
      mapper (newNums, numsToAdd) crtNum = 
        if (crtNum == 0) 
          then (6:newNums, 8:numsToAdd)
          else ((crtNum - 1):newNums, numsToAdd)
      (nextOfCrt, toAdd) = mapTuple reverse $ foldl mapper ([], []) crtNums
  in  nextOfCrt <> toAdd

mapTuple f (a, b) = (f a, f b)

