import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.List as L

import Control.Monad.State.Lazy

main = part2

part2 = do
  lines <- readLines "input.txt"
  let completionScores = do
      line <- lines
      let (firstBad, stack) = runState (findFirstIllegal line) ""
      case firstBad of 
        Nothing -> return $ completionPoints stack
        Just _ -> []
  let cs = L.sort completionScores
  let size = length completionScores
  _ <- print $ cs !! (size `div` 2)
  return ()

completionPoints :: String -> Int
completionPoints = foldl (\acc crt -> acc * 5 + pointsForOpening crt) 0

closingOf :: Char -> Char
closingOf '(' = ')'
closingOf '[' = ']'
closingOf '{' = '}'
closingOf '<' = '>'

closingPoints :: Char -> Int
closingPoints ')' = 1
closingPoints ']' = 2
closingPoints '}' = 3
closingPoints '>' = 4

pointsForOpening :: Char -> Int
pointsForOpening = closingPoints . closingOf

part1 = do
  lines <- readLines "input.txt"
  let corrupted = do
      line <- lines
      let firstIllegal = runFindFirstIllegal line
      case firstIllegal of  
        Just c -> [c]
        Nothing -> []
  _ <- print $ sum $ map points corrupted
  return ()

points :: Char -> Int
points ')' = 3
points ']' = 57
points '}' = 1197
points '>' = 25137

runFindFirstIllegal :: String -> Maybe Char
runFindFirstIllegal s = evalState (findFirstIllegal s) ""

findFirstIllegal :: String -> State String (Maybe Char)
findFirstIllegal [] = return Nothing
findFirstIllegal (c:cs) = do
  isCrtIllegal <- isIllegal c
  if isCrtIllegal 
    then return $ Just c
    else findFirstIllegal cs

isIllegal :: Char -> State String Bool
isIllegal crtChar = do
  myStack <- get
  if crtChar `elem` openingChars
    then do
      put (crtChar:myStack)
      return False
    else
      if null myStack
        then return True
        else do
          let (first:rest) = myStack
          if openingOf crtChar == first
            then do
              put rest
              return False
            else return True

openingChars = "([{<"

openingOf :: Char -> Char
openingOf ')' = '('
openingOf ']' = '['
openingOf '>' = '<'
openingOf '}' = '{'

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
