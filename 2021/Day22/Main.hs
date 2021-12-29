{-# LANGUAGE FlexibleInstances #-}

import qualified Data.Text as T

import Data.Map (Map)
import Data.Set (Set)

import Data.Maybe (maybeToList)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

main = part2

part1 = do
  lines <- readLines "input.txt"
  let commands = map parseLine lines
  -- print commands
  let enabled = applyCommands $ cropSpace commands
  print $ S.size enabled
  return ()

computeV1 :: [Command] -> Int
computeV1 cs = S.size $ applyCommands cs

computeV2 :: [Command] -> Int
computeV2 cs = allVolume $ foldl composeAllCommands [] cs

allLines :: Show a => [a] -> String
allLines xs = L.intercalate "\n" (map show xs)

part2 = do
  lines <- readLines "example.txt"
  let commands = map parseLine lines
  print $ computeV1 commands
  print $ computeV2 commands
  putStrLn . allLines $ foldl composeAllCommands [] commands
  return ()

allVolume :: [Command] -> Int
allVolume = foldl (\acc crt -> acc + (if stateOf crt == On then volume (areaOf crt) else (- volume (areaOf crt)))) 0 

volume :: Cuboid -> Int
volume c = len (x c) * len (y c) * len (z c)

len :: (Int, Int) -> Int
len (a, b) = abs (b - a) + 1

composeAllCommands :: [Command] -> Command -> [Command]
composeAllCommands cs crt = 
  let removeCommands = do
        a <- cs
        composeCommands a crt
      last = [crt | stateOf crt == On]
  in  cs <> removeCommands <> last

composeCommands :: Command -> Command -> [Command]
composeCommands c1 c2 = 
  let intersection = intersect (areaOf c1) (areaOf c2)
      rstate | stateOf c1 == On && stateOf c2 == On = [Off]
             | stateOf c1 == Off && stateOf c2 == On = [On]
             | stateOf c1 == On && stateOf c2 == Off = [Off]
             | stateOf c1 == Off && stateOf c2 == Off = [On]
      x = maybeToList intersection
      r = do
            s <- rstate
            a <- x
            return (Command s a)
  in  r
     
class HyperSegment a where
  intersect :: a -> a -> Maybe a

-- a  b  c  d
-- c  d  a  b
-- a  c  b  d
-- a  c  d  b
-- c  a  d  b
-- c  a  b  d
instance HyperSegment (Int, Int) where
  intersect (a, b) (c, d) = 
    if a > d || b < c 
      then Nothing
      else Just (max a c, min b d)

instance HyperSegment Cuboid where
  intersect c1 c2 = do
    xi <- x c1 `intersect` x c2
    yi <- y c1 `intersect` y c2
    zi <- z c1 `intersect` z c2
    return $ Cuboid xi yi zi

cropSpace :: [Command] -> [Command]
cropSpace [] = []
cropSpace ((Command state (Cuboid q w e)):rest) = 
  let c = Command state (Cuboid (cropInterval q) (cropInterval w) (cropInterval e))
  in  c : cropSpace rest

cropInterval :: (Int, Int) -> (Int, Int)
cropInterval (a, b) = (max (-50) a, min 50 b)

data Cuboid = Cuboid {
  x :: (Int, Int),
  y :: (Int, Int),
  z :: (Int, Int)
} deriving (Show)

data State = On | Off deriving (Show, Eq)

data Command = Command State Cuboid deriving (Show)

stateOf :: Command -> State
stateOf (Command s _) = s

areaOf :: Command -> Cuboid
areaOf (Command _ c) = c

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