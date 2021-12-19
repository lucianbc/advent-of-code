import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.List as L

import Control.Monad.State.Lazy

import Debug.Trace

main = part2

part2 = runComp evaluate
part1 = runComp versionSum

runComp :: (Show a) => (Packet -> a) -> IO ()
runComp comp = do
  line <- head <$> readLines "input.txt"
  let p = runParsePacket . hexToBinary $ line
  print $ comp p
  return ()

versionSum :: Packet -> Int
versionSum p = version p + case value p of
  Operator packets -> sum $ map versionSum packets
  Literal _ -> 0

evaluate :: Packet -> Int
evaluate (Packet _ typ value) = case value of
  Literal x -> x
  Operator packets ->
    let operatorFn 
          | typ == 0 = sum
          | typ == 1 = product
          | typ == 2 = minimum
          | typ == 3 = maximum
          | typ == 5 = (\[a,b] -> if a > b then 1 else 0)
          | typ == 6 = (\[a,b] -> if a < b then 1 else 0)
          | typ == 7 = (\[a,b] -> if a == b then 1 else 0)
    in  operatorFn $ map evaluate packets

data Packet = Packet {
  version :: Int,
  typ :: Int,
  value :: PacketValue
} deriving (Show)

data PacketValue = 
  Operator { packets :: [Packet] } | 
  Literal { num :: Int } 
  deriving (Show)

consume :: Int -> State String String
consume x = do
  line <- get
  let result = take x line
  put $ drop x line
  return result

parsePackets :: State String [Packet]
parsePackets = do
  line <- get
  if line == ""
    then return []
    else do
      crtPak <- parsePacket
      rest <- parsePackets
      return (crtPak:rest)

runParsePackets :: String -> [Packet]
runParsePackets = evalState parsePackets

parseNPackets :: Int -> [Packet] -> State String [Packet]
parseNPackets n packetsSoFar = 
  if length packetsSoFar == n 
    then return packetsSoFar 
    else do
      newPacket <- parsePacket
      parseNPackets n (packetsSoFar <> [newPacket])

parseOperator :: State String PacketValue
parseOperator = do
  (opType, length) <- parseOperatorHeader
  packets <- if opType == 0 
    then do
      chuckToParse <- consume length
      let parsedChunks = runParsePackets chuckToParse
      return parsedChunks
    else parseNPackets length []
  return (Operator packets)

parseOperatorHeader :: State String (Int, Int)
parseOperatorHeader = do
  lengthBit <- consume 1
  let (t, toConsume) = if head lengthBit == '0' then (0, 15) else (1, 11)
  bits <- consume toConsume
  return (t, toDec bits)

parsePacket :: State String Packet
parsePacket = do
  version <- parseNumber 3
  typ <- parseNumber 3
  value <- if typ == 4 then parseLiteral else parseOperator
  return $ Packet version typ value

runParsePacket :: String -> Packet
runParsePacket = evalState parsePacket

parseLiteral :: State String PacketValue
parseLiteral = do
  line <- get
  let (bits, tail) = takeType4 line
  put tail
  return $ Literal $ toDec bits

takeType4 :: String -> (String, String)
takeType4 s =
  let first5 = take 5 s
      numBits = tail first5
  in  if head first5 == '0'
    then (numBits, drop 5 s)
    else
      let (nextGroup, tail) = takeType4 $ drop 5 s
      in  (numBits <> nextGroup, tail)

parseNumber :: Int -> State String Int
parseNumber n = do
  bits <- consume n
  return $ toDec bits  

-- compute binary to decimal
toDec :: String -> Int
toDec = foldl (\acc crt -> 2 * acc + charToInt crt) 0

hexToBinary :: String -> String
hexToBinary = concatMap toBinary

toBinary :: Char -> String
toBinary '0' = "0000"
toBinary '1' = "0001"
toBinary '2' = "0010"
toBinary '3' = "0011"
toBinary '4' = "0100"
toBinary '5' = "0101"
toBinary '6' = "0110"
toBinary '7' = "0111"
toBinary '8' = "1000"
toBinary '9' = "1001"
toBinary 'A' = "1010"
toBinary 'B' = "1011"
toBinary 'C' = "1100"
toBinary 'D' = "1101"
toBinary 'E' = "1110"
toBinary 'F' = "1111"

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
-- part 1 - worked like a charm - so satisfying
-- part 2 - I was having an issue when instantiating the literal - 
--          I was converting the whole line
-- Learned - parsing, turning bin to dec
