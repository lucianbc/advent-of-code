import qualified Data.Text as T

import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

main = do
  lines <- readLines "input.txt"
  let commands = map parseLine lines
  -- print commands
  let enabled = applyCommands commands
  print $ S.size enabled
  return ()



data Command = On {
  x :: (Int, Int),
  y :: (Int, Int),
  z :: (Int, Int)
} | Off {
  x :: (Int, Int),
  y :: (Int, Int),
  z :: (Int, Int)
} deriving (Show)

applyCommands :: [Command] -> Set (Int, Int, Int)
applyCommands = foldl handleCommand S.empty

expandCommand :: Command -> Set (Int, Int, Int)
expandCommand c = expand (x c) (y c) (z c)

handleCommand :: Set (Int, Int, Int) -> Command -> Set (Int, Int, Int)
handleCommand crtSet cmd =
  let workingSet = expandCommand cmd
      result = case cmd of On {} -> crtSet `S.union` workingSet
                           Off {} -> crtSet `S.difference` workingSet
  in  result

parseLine :: String -> Command
parseLine s = 
  let [typ, rest] = splitOn " " s
      rangesStr = splitOn "," rest
      toInterval :: String -> (Int, Int)
      toInterval s = 
        let [n1, n2] = map toInt $ splitOn ".." (tail . tail $ s)
        in  if n1 < n2 then (n1, n2) else (n2, n1)
      [xRange, yRange, zRange] = map toInterval rangesStr
      cmdConstructor = if typ == "on" then On else Off
  in cmdConstructor xRange yRange zRange

expand :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Set (Int, Int, Int)
expand xrange yrange zrange = 
  let arr = do
        x <- toArr xrange
        y <- toArr yrange
        z <- toArr zrange
        return (x, y, z)
  in S.fromList arr

toArr :: (Int, Int) -> [Int]
toArr (a, b) = 
  let left = max (-50) a
      right = min 50 b
  in  [left..right]

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
