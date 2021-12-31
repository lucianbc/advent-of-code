import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.List as L
import Control.Monad.State.Lazy
import Debug.Trace

allLines :: Show a => [a] -> String
allLines xs = L.intercalate "\n" (map show xs)

runParseNumber :: String -> Element
runParseNumber = evalState parseNumber

parseNumber :: State String Element
parseNumber = do
  crt <- consume
  if crt == '[' 
    then do
      left <- parseNumber
      _ <- consume
      right <- parseNumber
      _ <- consume
      return $ Pair left right
    else return $ Regular (charToInt crt)

consume :: State String Char
consume = do
  buffer <- get
  let crt = head buffer
  put $ tail buffer
  return crt

main = part2

part1 = do
  lines <- readLines "input.txt"
  let numbers = map runParseNumber lines
  let result = foldl1 addition numbers
  print $ magnitude result
  return ()

part2 = do
  numbers <- map runParseNumber <$> readLines "example.txt"
  let pairs = any2 numbers
  let magnitudes = map (magnitude . uncurry addition) pairs
  print $ maximum magnitudes
  return ()

any2 :: [Element] -> [(Element, Element)]
any2 xs = do
  x <- xs
  y <- xs
  if (x == y) then [] else [(x, y)]

magnitude :: Element -> Int
magnitude (Regular x) = x
magnitude (Pair l r) = 3 * (magnitude l) + 2 * (magnitude r)

explodeStr = explode . runParseNumber

testReduce = do
  -- let x = runParseNumber "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"
                     --  "[[[[0,[4,5]],[0,4]],[[0,[7,6]],[9,5]]],      [10,[[0,[11,3]],[[6,3],[8,8]]]]]"
  -- let x = runParseNumber "[[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]"
                     --  "[[[[0,[4,5]],[0,4]],[[0,[7,6]],[9,5]]],      [10,[[0,[11,3]],[[6,3],[8,8]]]]]"
                     --  "[[[[0,[4,5]],[0,4]],[[0,[7,6]],[9,5]]],      [10,[[0,[11,3]],[[6,3],[8,8]]]]]"
  -- print $ reduce x

  let x = runParseNumber "[[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]"
  -- let x = runParseNumber "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
  print $ reduce x
  return ()

addition :: Element -> Element -> Element
addition a1 a2 = reduce $ Pair a1 a2

dreduce :: Element -> [(String, Element)]
dreduce x =
  let exploded = explode x
      splitted = split x
  in  if exploded /= x
        then ("E", exploded) : (dreduce exploded)
        else
          if splitted /= x
            then ("S", splitted) : (dreduce splitted)
            else []

reduce :: Element -> Element
reduce x = 
  let exploded = explode x
      splitted = split x
  in  if exploded /= x
        then reduce exploded
        else
          if splitted /= x
            then reduce splitted
            else x

testSplit = do
  -- print $ split (Regular 10)
  -- print $ split (Regular 11)
  -- print $ split (Regular 12)
  -- let eqSample = Pair (Regular 5) (Regular 6)
  -- print $ split eqSample
  -- print $ split eqSample == eqSample
  let doubleSplit = explode $ runParseNumber "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]"
  print doubleSplit
  print $ split doubleSplit -- [[[[0,7],4],[[7,8],[0,13]]],[1,1]]
  return ()

pexplode t = do
  let x = explode t
  print x
  return x

psplit t = do
  let x = split t
  print x
  return x

debugReduce = do
  let s0 = runParseNumber "[[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]"
  -- let s0 = runParseNumber "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"
  let r = dreduce s0
  putStrLn $ allLines r

testExplode = do
  -- print $ explodeStr "[[[[[9,8],1],2],3],4]" -- [[[[0,9],2],3],4]
  -- print $ explodeStr "[7,[6,[5,[4,[3,2]]]]]" -- [7,[6,[5,[7,0]]]]
  -- print $ explodeStr "[[6,[5,[4,[3,2]]]],1]" -- [[6,[5,[7,0]]],3]
  -- print $ explodeStr "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" -- [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]
  -- print $ explodeStr "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]" -- [[3,[2,[8,0]]],[9,[5,[7,0]]]  
  -- print $ explodeStr "[3,4]"
  -- print $ explodeStr "[[[[[1,1],[2,2]],[3,3]],[4,4]],[5,5]]" -- [[[[0,[3,2]],[3,3]],[4,4]],[5,5]]
  -- print $ explodeStr "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]" -- [[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
  -- let x = runParseNumber "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
  -- print $ explodeHepler x 1
  
  let s0 = runParseNumber "[[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]"
  print s0
  s1 <- pexplode s0
  s2 <- pexplode s1
  s3 <- pexplode s2
  s4 <- psplit s3
  s5 <- pexplode s4
  s6 <- psplit s5
  s7 <- pexplode s6
  s8 <- pexplode s7
  s9 <- pexplode s8
  
  return ()
  -- print $ explodeStr "[[[[4,0],[5,0]],[[[4,5],[2,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]"


split :: Element -> Element
split x = case splitHelper x of Unsplitted a -> a
                                Splitted a -> a

splitHelper :: Element -> SplitData
splitHelper (Regular x) = 
  if x >= 10 
    then Splitted (Pair (Regular (x `div` 2)) (Regular ((x + 1) `div` 2)))
    else Unsplitted (Regular x)
splitHelper (Pair l r) = 
  let left = splitHelper l
      right = splitHelper r
  in  case (left, right) of (Splitted x, _) -> Splitted (Pair x r)
                            (Unsplitted _, Unsplitted _) -> Unsplitted (Pair l r)
                            (Unsplitted _, Splitted y) -> Splitted (Pair l y) 
                   

data SplitData = Splitted Element | Unsplitted Element deriving (Eq, Show)

data Element = Regular Int | Pair Element Element deriving (Eq)

instance Show Element where
  show (Pair x y) = "[" ++ show x ++ "," ++ show y ++ "]"
  show (Regular x) = show x

explode :: Element -> Element
explode element = case explodeHepler element 0 of
  Id x -> x
  Updated x -> x
  RippleRightWith _ x -> x
  RippleLeftWith _ x -> x

data ExplodeResult = 
  RippleBoth Int Int
  | Id Element
  | Updated Element
  | RippleLeftWith Int Element
  | RippleRightWith Int Element
  deriving (Show, Eq)

explodeHepler :: Element -> Int -> ExplodeResult
explodeHepler (Pair (Regular l) (Regular r)) 4 = RippleBoth l r
explodeHepler (Pair (Regular l) (Regular r)) _ = Id (Pair (Regular l) (Regular r))
explodeHepler (Regular x) _ = Id (Regular x)
explodeHepler (Pair left right) d = 
  let 
      leftExplode_ = explodeHepler left (d + 1)
      rightExplode_ = explodeHepler right (d + 1)
      leftExplode = explodeHepler left (d + 1)
      rightExplode = explodeHepler right (d + 1)
      -- leftExplode = trace ("Left At depth " ++ show d ++ " I have " ++ show leftExplode_) leftExplode_
      -- rightExplode = trace ("Right At depth " ++ show d ++ " I have " ++ show rightExplode_) rightExplode_
      result = case (leftExplode, rightExplode, left, right) of
                -- [[a, b], c] -> ripple left because I already consumed the right ripple, I transform it to (ripple a [0, b + c])
                (RippleBoth rl rr, _, _, Regular r) -> RippleLeftWith rl (Pair (Regular 0) (Regular (rr + r)))
                -- [[a, b], [c, d]] -> ripple left because I already consumed the right ripple, I transform it to (ripple a [b + c, d])
                (RippleBoth a b, _, _, cd) -> RippleLeftWith a (Pair (Regular 0) (addToLeftmost cd b))
                -- already consumed full left update
                (Updated l, _, ab, cd) -> Updated (Pair l cd)
                -- [a, [b, c]] -> ripple right because 
                (Id _, RippleBoth b c, Regular a, _) -> RippleRightWith c (Pair (Regular (a + b)) (Regular 0))
                (Id l, Id r, _, _) -> Id (Pair l r)
                (Id l, Updated r, _, _) -> Updated (Pair l r)
                -- [[7, [[6, 5], 4]], 3] -> [[13, [0, 9]], 3]
                (RippleLeftWith reminder innerPair, _, _, r) -> RippleLeftWith reminder (Pair innerPair r)
                (RippleRightWith reminder innerPair, _, _, r) -> Updated (Pair innerPair (addToLeftmost r reminder))
                (_, RippleLeftWith reminder innerPair, l, _) -> Updated (Pair (addToRightmost l reminder) innerPair)
                (_, RippleRightWith reminder innerPair, l, _) -> RippleRightWith reminder (Pair l innerPair)
                _ -> error ("not handled case " ++ show (leftExplode, rightExplode, left, right))
  in  result

addToLeftmost :: Element -> Int -> Element
addToLeftmost (Regular x) toAdd = Regular (x + toAdd)
addToLeftmost (Pair (Regular l) r) toAdd = Pair (Regular (l + toAdd)) r
addToLeftmost (Pair x y) toAdd = Pair (addToLeftmost x toAdd) y

addToRightmost :: Element -> Int -> Element
addToRightmost (Regular x) toAdd = Regular (x + toAdd)
addToRightmost (Pair l (Regular r)) toAdd = Pair l (Regular (r + toAdd))
addToRightmost (Pair x y) toAdd = Pair x (addToRightmost y toAdd)

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
