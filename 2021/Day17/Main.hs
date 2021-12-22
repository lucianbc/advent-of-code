import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.List as L

import Data.Ord (comparing)

import Debug.Trace

main = do
  print $ part1 (144, 178) (-100, -76)
  return ()

part1 :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
part1 xBounds yBounds = 
  let speeds = do
        v0x <- [0..(snd xBounds)]
        v0y <- [fst yBounds..(- fst yBounds)]
        maxHeight <- simulateTrajectory xBounds yBounds v0x v0y
        return (maxHeight, (v0x, v0y))
      maxHeight = L.maximumBy (comparing (fst . fst)) $ trace ("speeds is " ++ show (length $ map snd speeds)) speeds
  in  maxHeight

between :: Int -> (Int, Int) -> Bool
between x (a, b) = x >= a && x <= b

simulateTrajectory :: (Int, Int) -> (Int, Int) -> Int -> Int -> [(Int, Int)]
simulateTrajectory boundsX boundsY v0x v0y =
  let simulateRec :: Int -> Int -> Int -> Int -> Int -> Int -> [(Int, Int)]
      simulateRec vx vy crtX crtY maxY step =
        let inBounds_ = crtX `between` boundsX && crtY `between` boundsY
            inBounds = inBounds_
            -- inBounds = trace ("in bounds with " ++ show (crtX `between` boundsX) ++ ", " ++ show (crtY `between` boundsY)) inBounds_
            pastBounds_ = crtX > snd boundsX || crtY < fst boundsY
            pastBounds = pastBounds_
            -- pastBounds = trace ("past bounds with " ++ show crtX ++ ", " ++ show crtY ++ " -> " ++ show pastBounds_) pastBounds_
            newCrtY = crtY + vy
            newCrtX = crtX + vx
            newVy = vy - 1
            newVx = if vx - 1 < 0 then 0 else vx - 1
            newMax = max maxY newCrtY
            result | inBounds = [(maxY, step)]
                   | pastBounds = []
                   | otherwise = simulateRec newVx newVy newCrtX newCrtY newMax (step + 1)
        in  result
  in  simulateRec v0x v0y 0 0 0 0

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
