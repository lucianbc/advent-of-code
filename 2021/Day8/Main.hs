import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as S

main = part11

part1 = do
  lines <- readLines "input.txt"
  let x = do
            line <- lines
            let elems = snd . parseLine $ line
            filter (\x -> length x `elem` targetLengths) elems 
  _ <- print $ length x
  let codes = concat $ map (snd . parseLine) lines
  _ <- print $ length codes
  --_ <- print codes
  let ct = count (\x -> length x `elem` targetLengths) codes
  print ct

part11 = do
  lines <- readLines "input.txt"
  let allSecondParts = concat $ map (snd . parseLine) lines
  _ <- print allSecondParts
  _ <- print $ length allSecondParts
  -- _ <- print $ (S.size . S.fromList . filter hasExpectedSegments) allSecondParts
  _ <- print $ length $ filter hasExpectedSegments allSecondParts
  return ()


targetLengths :: [Int]
targetLengths = [2, 3, 4, 7]

hasExpectedSegments :: String -> Bool
hasExpectedSegments s = length s `elem` targetLengths

count :: (a -> Bool) -> [a] -> Int
count pred ls = foldl (\acc crt -> if (pred crt) then acc + 1 else acc + 0) 0 ls

parseLine :: String -> ([String], [String])
parseLine line = 
  let [part1, part2] = map words $ splitOn " | " line
  in  (part1, part2)

splitOn :: String -> String -> [String]
splitOn delim = map T.unpack . T.splitOn (T.pack delim) . T.pack

toInt :: String -> Int
toInt = read

readLines :: FilePath -> IO [String]
readLines = fmap lines <$> readFile
