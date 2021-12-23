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

type Segment = (Int, Int)
data Point3 = Point3 {
  x :: Int,
  y :: Int,
  z :: Int
} deriving (Show)
type Cuboid = (Point3, Point3)

class Range a where
  intersect :: a -> a -> Maybe a
  area :: a -> Int

-- a1        a2   b1          b2 -> no intersect

-- b1      b2  a1     a2       --> no intersect 

-- b1       a1  b2       a2

-- a1        b1     a2         b2
instance Range (Int, Int) where
  intersect (a1, a2) (b1, b2)
    | b1 > a2 || a1 > b2 = Nothing
    | otherwise          = Just (max a1 b1, min a2 b2)
  area (a1, a2) = a2 - a1 + 1

instance Range Cuboid where
  intersect (a, b) = do
    (x1, x2) <- intersect (x a, x b)
    (y1, y2) <- intersect (y a, y b)
    (z1, z2) <- intersect (z a, z b)
    return $ (Point3 x1 y1 z1, Point3 x2 y2 z2)

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
