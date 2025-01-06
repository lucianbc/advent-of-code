import Control.DeepSeq (deepseq, force)
import Data.Char (digitToInt)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe (listToMaybe)
import Data.Maybe qualified as L
import Debug.Trace (trace, traceShow, traceShowId)
import System.Environment

main = do
  lines <- readInput
  part <- readPart
  let result = case part of
        "part1" -> part1 lines
        "part2" -> part2 lines
        _ -> "Bad part argument. Must be \"part1\" or \"part2\""
  putStrLn $ "Result is \n" ++ force result

part1 :: [String] -> String
part1 lines =
  let nums = map digitToInt (head lines)
      slots = memoryLayout nums
      fbs = fileBlocks nums
      deque = createDeque fbs
      out = fillMemory slots deque
      outPrinted = map (head . show) out
   in show $ csum out 0 0

fillMemory :: [Slot] -> BlocksDeque -> [Int]
fillMemory slots initialQueue =
  let slotsOfLength1 = do
        s <- slots
        let al = case s of
              Fill l _ -> l
              Gap l -> l
        replicate al s
      fillMemoryRec :: [Slot] -> BlocksDeque -> [Int]
      fillMemoryRec [] _ = []
      fillMemoryRec (x : xs) crtQueue =
        let queueExtractor = case x of
              Fill _ _ -> consumeHead
              Gap _ -> consumeTail
            nextIter = queueExtractor crtQueue
         in case nextIter of
              Nothing -> []
              Just (newQueue, num) -> num : fillMemoryRec xs newQueue
   in fillMemoryRec slotsOfLength1 initialQueue

part2 :: [String] -> String
part2 lines =
  "todo"

memoryLayout :: [Int] -> [Slot]
memoryLayout = memoryLayout_ 0
  where
    memoryLayout_ _ [] = []
    memoryLayout_ pos [x] = []
    memoryLayout_ pos (fill : skip : rest) = Fill fill pos : Gap skip : memoryLayout_ (pos + 1) rest

fileBlocks :: [Int] -> [(Int, Int)]
fileBlocks = fileBlocksInternal 0
  where
    fileBlocksInternal :: Int -> [Int] -> [(Int, Int)]
    fileBlocksInternal _ [] = []
    fileBlocksInternal fileId (take : rest) =
      (take, fileId)
        : if null rest
          then []
          else fileBlocksInternal (fileId + 1) (tail rest)

csum :: [Int] -> Int -> Integer -> Integer
csum [] _ acc = acc
csum (x : xs) pos acc = csum xs (pos + 1) acc + toInteger (x * pos)

data Slot = Fill Int Int | Gap Int deriving (Show, Eq)

data BlocksDeque = BlocksDeque
  { headIndex :: Int,
    tailIndex :: Int,
    elems :: M.Map Int Int
  }
  deriving (Show)

createDeque :: [(Int, Int)] -> BlocksDeque
createDeque elems =
  let m = M.fromList (map (\(a, b) -> (b, a)) elems)
   in BlocksDeque 0 (length elems - 1) m

isEmpty :: BlocksDeque -> Bool
isEmpty b = headIndex b > tailIndex b

consumeHead :: BlocksDeque -> Maybe (BlocksDeque, Int)
consumeHead b = do
  nextHeadIndex <- L.find (\ix -> elems b M.! ix > 0) [headIndex b .. tailIndex b]
  let newDeq = BlocksDeque nextHeadIndex (tailIndex b) (M.adjust (\x -> x - 1) nextHeadIndex (elems b))
  return (newDeq, nextHeadIndex)

consumeTail :: BlocksDeque -> Maybe (BlocksDeque, Int)
consumeTail b = do
  nextTailIndex <- L.find (\ix -> elems b M.! ix > 0) [tailIndex b, tailIndex b - 1 .. 0]
  let newDeq = BlocksDeque (headIndex b) nextTailIndex (M.adjust (\x -> x - 1) nextTailIndex (elems b))
  return (newDeq, nextTailIndex)

swapHead :: [a] -> a -> [a]
swapHead (a : as) x = x : as

readPart :: IO String
readPart = do
  args <- getArgs
  return $ args !! 1

readInput :: IO [String]
readInput = do
  args <- getArgs
  let filePath = head args
  inputContent <- readFile filePath
  return $ lines inputContent
