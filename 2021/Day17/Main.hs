import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.List as L

import Data.Ord (comparing)

import Debug.Trace

main = do
  print $ part1 (144, 178) (-100, -76)
  return ()

renderEq :: (Int, Int, Int) -> String
renderEq (vx, vy, t) = (show (vx, vy, t)) ++ "\n" ++ "x: " ++ (show $ computeEq vx t) ++ "\n" ++ "y: " ++ (show $ computeEq vy t) ++ "\n\n"

-- computeEqWithStop :: Int -> Int -> Int
-- computeEqWithStop x y

part1 :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
part1 xBounds yBounds = 
  let speeds = do
        -- let v0x = 6
        -- let v0y = 0
        v0x <- [0..(snd xBounds)]
        v0y <- [fst yBounds..(- fst yBounds)]
        maxHeight <- simulateTrajectory xBounds yBounds v0x v0y
        return (maxHeight, (v0x, v0y))
      maxHeight = L.maximumBy (comparing (fst . fst)) $ trace ("speeds is " ++ show (length $ map snd speeds)) speeds
      -- maxHeight = L.maximumBy (comparing (fst . fst)) speeds
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

computeEq :: Int -> Int -> Int
computeEq v0 t = v0 * t - (t * (t - 1)) `div` 2

findVelocitiesForArea :: [Int] -> [Int] -> [(Int, Int, Int)]
findVelocitiesForArea xRange yRange = do
  -- x <- [1..last xRange]
  let x = 21
  y <- yRange
  (vx, t) <- findXVelocitiesAndTimes x
  vxfinal <- findIntVelocity x t

  
  vy <- findIntVelocity y t
  return (vx, vy, t)

-- I actually need to find the x velocities where I get on 0 somewhere along the x area
-- Then I need to find the y velocity such that 

-- if final velocity x is 0, then I can take max time to get to that y spot
-- so I can find all velocities and times for that spot on y and keep only the ones with time >= y
-- but I don't have an upper limit for the velocities that I test

over :: Int -> Int -> Float
over a b = fromIntegral a / fromIntegral b

findIntVelocity :: Int -> Int -> [Int]
findIntVelocity s t = 
  let r = (2 * s + t * t - t) `over`  (2 * t)
  in  [round r | isInt r 10]

-- s = v * t - (t^2 - t) / 2
-- 2s = 2vt - (t^2 - t)
-- 2s + (t^2 - t) = 2vt
-- v = (2s + t^2 - t) / 2t

findVelocitiesForPosition :: Int -> Int -> [(Int, Int, Int)]
findVelocitiesForPosition x y = do
  xVelocities <- findXVelocitiesAndTimes x
  (vx, vy, t) <- canDoOnY y xVelocities
  return (vx, vy, t)

canDoOnY :: Int -> (Int, Int) -> [(Int, Int, Int)]
canDoOnY y (vx, time) = 
  let vy = fromIntegral (time ^ 2 - time + 2 * y) / (2)
  in  [(vx, round vy, time) | isInt vy 10]

findXVelocitiesAndTimes :: Int -> [(Int, Int)]
findXVelocitiesAndTimes s = do
  v0 <- [1..s]
  time <- findIntTimeAtX v0 s (-1)
  return (v0, time)

findIntTimeAtX :: Int -> Int -> Int -> [Int]
findIntTimeAtX v0 s a =
  let b = fromIntegral (2 * v0 - a)
      c = fromIntegral (-2 * s)
      d = b ^ 2 - fromIntegral (4 * a * c)
      sd = sqrt d
      a2 = fromIntegral $ 2 * a
      t1 = (-b - sd) / a2
      t2 = (-b + sd) / a2
      positives = filter (> 0) [t1, t2]
      integers = filter (`isInt` 10) positives
  in  [round (minimum integers) | not (d < 0 || null integers)]



-- 2s = 2v0 * t + a * t * t
-- at ^ 2 + 2v0t - 2s = 0
-- d = 4v0^2 + 8as
-- t1 = (-v0 - sqrt (v0^2 + 2as))
-- t2 = (-v0 + sqrt (v0^2 + 2as))

isInt :: (Integral a, RealFrac b) => b -> a -> Bool
isInt x n = (round $ 10^(fromIntegral n)*(x-(fromIntegral $ round x)))==0

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
