{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use uncurry" #-}
import Control.DeepSeq (deepseq, force)
import Data.Char (digitToInt)
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe (listToMaybe)
import Data.Maybe qualified as L
import Data.Maybe qualified as M
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
  let nums = map digitToInt (head lines)
      slots = memoryLayout nums
      tilingMap = buildMapOfSlots slots
      upperTilingLimit = tilingMap |> M.toDescList |> head |> fst |> (+ 1)
      sim = simulate upperTilingLimit tilingMap
   in sim |> last |> M.toAscList |> flip csum3 0 |> show

simulate :: Int -> M.Map Int Slot -> [M.Map Int Slot]
simulate limit crtMap =
  let next = performMove limit crtMap
   in case next of
        Nothing -> [crtMap]
        Just (nextLimit, nextMap) -> crtMap : simulate nextLimit nextMap

printSlotsMap :: M.Map Int Slot -> String
printSlotsMap m = M.toAscList m |> map snd |> printSlots

printSlots :: [Slot] -> String
printSlots =
  foldr
    ( \x ->
        (++)
          ( case x of
              Fill l n -> replicate l ((head . show) n)
              Gap l -> replicate l '.'
          )
    )
    ""

lastFillBefore :: Int -> M.Map Int Slot -> Maybe (Int, Slot)
lastFillBefore upperLimit slots =
  let (slotsBeforeLimit, _) = M.split upperLimit slots
   in L.find (isFill . snd) (M.toDescList slotsBeforeLimit)

firstGapAccepting :: (Int, Slot) -> M.Map Int Slot -> Maybe (Int, Slot)
firstGapAccepting (fillIndex, fillElem) slots =
  let isValidGap (p, g) = isGap g && p < fillIndex && sLen g >= sLen fillElem
   in L.find isValidGap $ M.toAscList slots

findGapAndFollowingElemFor :: (Int, Slot) -> M.Map Int Slot -> Maybe (Int, Slot, Maybe (Int, Slot))
findGapAndFollowingElemFor pos map = findGapAndFollowingElemFor_ pos (M.toAscList map)
  where
    findGapAndFollowingElemFor_ _ [] = Nothing
    findGapAndFollowingElemFor_ reference@(fillIndex, fillElem) (y@(p, g) : xs) =
      -- traceShow ("comparing " ++ show reference ++ " with " ++ show y) $
      if isGap g && p < fillIndex && sLen g >= sLen fillElem
        then Just (p, g, M.listToMaybe xs)
        else findGapAndFollowingElemFor_ reference xs

performMove :: Int -> M.Map Int Slot -> Maybe (Int, M.Map Int Slot)
performMove currentUpperLimit slots = do
  fillToConsider@(fillPos, fillToMove) <- lastFillBefore currentUpperLimit slots
  let dest = findGapAndFollowingElemFor fillToConsider slots
  case dest of
    Nothing -> Just (fillPos, slots) -- keep the same slots, but move the back position to the left
    Just (gapPos, gapSlot, nextSlotAfterGap) ->
      if sLen fillToMove == sLen gapSlot
        then -- Just swap the fill with the gap
          Just (fillPos, M.insert gapPos fillToMove $ M.insert fillPos gapSlot slots)
        else -- Fill is smaller than the gap

          let -- tilingWithoutTheSwapped = M.delete gapPos . M.insert fillPos (Gap (sLen fillToMove)) $ slots
              -- withTheFillAdded = M.insert gapPos fillToMove tilingWithoutTheSwapped
              withTheFillAdded =
                slots
                  |> M.delete gapPos
                  |> M.insert fillPos (Gap (sLen fillToMove))
                  |> M.insert gapPos fillToMove

              x = case nextSlotAfterGap of
                Nothing -> M.insert (gapPos + sLen fillToMove) (Gap (sLen gapSlot - sLen fillToMove)) withTheFillAdded
                Just (nextStart, nextSlot) ->
                  if isFill nextSlot
                    then M.insert (gapPos + sLen fillToMove) (Gap (sLen gapSlot - sLen fillToMove)) withTheFillAdded
                    else -- I have to merge nextSlot and the new gap

                      let newGap = Gap (sLen gapSlot - sLen fillToMove + sLen nextSlot)
                       in withTheFillAdded |> M.delete nextStart |> M.insert (gapPos + sLen fillToMove) newGap
           in Just (fillPos, x)

(|>) :: a -> (a -> b) -> b
(|>) self fn = fn self

-- gapPos = 4
-- sLen fillToMove = 3
-- sLen gapSlot = 5

-- at gapPos, I put Fill 3
-- at gapPos + sLen fillToMove, I put Gap (2)

-- let
--  in Just (fillPos, slots)

-- return (1, slots)

isFill x = case x of
  Fill _ _ -> True
  _ -> False

isGap x = case x of
  Gap _ -> True
  _ -> False

buildMapOfSlots :: [Slot] -> M.Map Int Slot
buildMapOfSlots s = buildMapOfSlots_ s 0 M.empty
  where
    buildMapOfSlots_ :: [Slot] -> Int -> M.Map Int Slot -> M.Map Int Slot
    buildMapOfSlots_ [] _ accMap = accMap
    buildMapOfSlots_ (x : xs) crtIndex accMap = buildMapOfSlots_ xs (crtIndex + sLen x) (M.insert crtIndex x accMap)

sLen :: Slot -> Int
sLen (Fill l _) = l
sLen (Gap l) = l

memoryLayout :: [Int] -> [Slot]
memoryLayout = memoryLayout_ 0
  where
    memoryLayout_ _ [] = []
    memoryLayout_ pos [x] = [Fill x pos]
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

csum3 :: [(Int, Slot)] -> Integer -> Integer
csum3 [] acc = acc
csum3 ((start, slot) : xs) acc =
  case slot of
    Gap _ -> csum3 xs acc
    Fill l numberInMem ->
      let indexes = map toInteger $ take l [start ..]
          toAdd = map (,numberInMem) indexes |> map (\(a, b) -> a * toInteger b) |> sum
       in csum3 xs (acc + toAdd)

-- csum2 :: [Char] -> Integer -> Integer -> Integer
-- csum2 [] _ acc = acc
-- csum2 (x : xs) pos acc = case x of
--   '.' -> csum2 xs (pos + 1) acc
--   _ -> csum2 xs (pos + 1) (acc + (toInteger . digitToInt $ x) * pos)

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
