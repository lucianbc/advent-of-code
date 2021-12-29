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
  let enabled = applyCommands $ cropSpace commands
  print $ S.size enabled
  return ()

cropSpace :: [Command] -> [Command]
cropSpace [] = []
cropSpace ((Command state (Cuboid q w e)):rest) = 
  let c = Command state (Cuboid (cropInterval q) (cropInterval w) (cropInterval e))
  in  c:(cropSpace rest)

cropInterval :: (Int, Int) -> (Int, Int)
cropInterval (a, b) = (max (-50) a, min 50 b)

data Cuboid = Cuboid {
  x :: (Int, Int),
  y :: (Int, Int),
  z :: (Int, Int)
} deriving (Show)

data State = On | Off deriving (Show)

data Command = Command State Cuboid deriving (Show)

applyCommands :: [Command] -> Set (Int, Int, Int)
applyCommands = foldl handleCommand S.empty

expandCommand :: Command -> Set (Int, Int, Int)
expandCommand (Command _ c) = expand (x c) (y c) (z c)

handleCommand :: Set (Int, Int, Int) -> Command -> Set (Int, Int, Int)
handleCommand crtSet cmd =
  let workingSet = expandCommand cmd
      result = case cmd of (Command On _) -> crtSet `S.union` workingSet
                           (Command Off _) -> crtSet `S.difference` workingSet
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
      state = if typ == "on" then On else Off
  in Command state (Cuboid xRange yRange zRange)

expand :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Set (Int, Int, Int)
expand xrange yrange zrange = 
  let arr = do
        x <- toArr xrange
        y <- toArr yrange
        z <- toArr zrange
        return (x, y, z)
  in S.fromList arr

toArr :: (Int, Int) -> [Int]
toArr (a, b) = [a..b]

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