import qualified Data.Text as T

import Data.Map (Map)

import qualified Data.Map as M

import qualified Data.List as L

import Debug.Trace

import Control.Monad.State.Lazy


main = part1

part1 = do
  lines <- readLines "input.txt"
  let (p, rules) = parseLines lines

  let finalCode = last . (take 10) $ allExpand p rules
  let counts = countEach finalCode
  
  let sortedCounts = L.sort $ map snd $ M.toList counts
  let result = last sortedCounts - head sortedCounts
  
  print result
  print counts
  return ()

countEach :: Ord a => [a] -> Map a Int
countEach = countRec M.empty
  where countRec :: Ord a => Map a Int -> [a] -> Map a Int
        countRec soFar [] = soFar
        countRec soFar (x:xs) = case (M.lookup x soFar) of 
          Just ct -> countRec (M.insert x (ct + 1) soFar) xs
          Nothing -> countRec (M.insert x 1 soFar) xs

allExpand :: String -> [Rule] -> [String]
allExpand p rules = 
  let nextPattern = expand p rules
  in  nextPattern : (allExpand nextPattern rules)

expand :: String -> [Rule] -> String
expand pattern rules = 
  let pairs = zip pattern $ tail pattern
      applyRules :: [Rule] -> (Char, Char) -> [Char]
      applyRules [] pair = [snd pair]
      applyRules (rule:rest) pair = if fst rule == pair then [snd rule, snd pair] else applyRules rest pair
      result = head pattern : (concat $ map (applyRules rules) pairs)
  in  result

type Memo = Map (Char, Char, Int) [Char]

-- expandPair :: Int -> (Char, Char) -> [Rule] -> State Memo [Char]
-- expandPair rounds pair rules = do
--   memo <- get
--   let x = M.lookup (fst pair, snd pair, rounds) memo
--   case x of 
--     Just r -> return r
--     Nothing -> do
--       let matchingRule = L.find (\rule -> fst rule == pair) rules
--       case matchingRule of 
--         Nothing -> do
--           put (fst pair, snd pair, rounds) [fst pair, snd pair]
--       return []

parseLines :: [String] -> (String, [Rule])
parseLines lines = 
  let initialPattern = head lines
      ruleStrings = tail . tail $ lines
      rules = do 
        ruleString <- ruleStrings
        let [[a, b], insert] = splitOn " -> " ruleString
        return ((a, b), head insert)
  in  (initialPattern, rules)

type Rule = ((Char, Char), Char)

isMatchedBy :: Rule -> (Char, Char) -> Bool
isMatchedBy (key, _) given = key == given

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

-- notes
-- part 1 - simple
