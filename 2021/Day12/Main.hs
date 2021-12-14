{-# LANGUAGE TupleSections #-}

import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as M

import Data.Char

import Data.Set (Set)
import qualified Data.Set as S

import qualified Data.List as L

import Control.Monad.State.Lazy

import Data.Maybe

import Debug.Trace

main = part1

part1 = do
  g <- parseGraph
  let allPaths = evalState (findAllPaths filterValidEdges "start" ) (g, S.empty, M.empty)
  _ <- print $ length allPaths
  return ()

part2 = do
  g <- parseGraph
  let allPaths = evalState (findAllPaths filterValidEdges2 "start" ) (g, S.empty, M.empty)
  _ <- print $ length allPaths
  return ()

parseGraph = do
  lines <- readLines "input.txt"
  let edges = map (splitOn "-") lines
  let graph = foldl (\acc [a, b] -> addEdge acc a b) M.empty edges
  return graph

type StateType = (Graph, Set (Node, Node), Map Node Int)

addEdgeToState :: (Node, Node) -> State StateType ()
addEdgeToState edge = do
  (g, e, n) <- get
  put (g, edge `S.insert` e, n)

addNodeToState :: Node -> State StateType ()
addNodeToState node = do
  (g, e, n) <- get
  let newVal = maybe 1 (+ 1) (M.lookup node n)
  put (g, e, M.insert node newVal n)

removeEdgeFromState :: (Node, Node) -> State StateType ()
removeEdgeFromState edge = do
  (g, e, n) <- get
  put (g, edge `S.delete` e, n)

removeNodeFromState :: Node -> State StateType ()
removeNodeFromState node = do
  (g, e, n) <- get
  let existingVal = (1 `subtract`) <$> M.lookup node n 
  let newNodeMap = case existingVal of Just 0 -> M.delete node n
                                       Just ct -> M.insert node ct n 
                                       Nothing -> n
  put (g, e, newNodeMap)

findAllPaths :: EdgeFilter -> Node -> State StateType [[Node]]
findAllPaths edgeFilter crtNode = do
  (graph, _, _) <- get
  let edges = map (crtNode,) $ graph M.! crtNode
  addNodeToState crtNode
  validEdges <- edgeFilter edges
  paths <- if null validEdges || isEnd crtNode
    then return [[crtNode] | isEnd crtNode]
    else do
          let statePaths = do
                edge <- validEdges
                let statePath = do
                      addEdgeToState edge
                      pathsFromHere <- findAllPaths edgeFilter $ snd edge
                      removeEdgeFromState edge
                      return pathsFromHere
                return statePath      
          nestedPaths <- sequence statePaths
          let allPahtsSoFar = concat nestedPaths
          let allPathsWithCrt = map (crtNode :) allPahtsSoFar
          return allPathsWithCrt
  removeNodeFromState crtNode
  return paths

type EdgeFilter = [(Node, Node)] -> State StateType [(Node, Node)]

filterValidEdges :: EdgeFilter
filterValidEdges edges = do
  (_, visitedEdges, visitedNodes) <- get
  let validEdges = do
        edge <- edges
        if edge `S.member` visitedEdges 
          then []
          else if isSmallCave (snd edge) && snd edge `M.member` visitedNodes
            then []
            else return edge 
  return validEdges

getCountOfCave :: Map String Int -> String -> Int
getCountOfCave map cave = Data.Maybe.fromMaybe 0 (M.lookup cave map)

isOtherWithTwoVisits :: Map String Int -> String -> Bool
isOtherWithTwoVisits map cave = 
  let pickCaves :: String -> Int -> Bool
      pickCaves otherCave visitedCount = isSmallCave otherCave && not (isStart otherCave) && not (isEnd otherCave) && (otherCave /= cave) && visitedCount > 1
      otherSmallVisited = M.filterWithKey pickCaves map
      result = M.size otherSmallVisited /= 0
      -- rr = trace ("check for other " ++ show map ++ ", " ++ show cave ++ ", " ++ show result) result
  in  result

filterValidEdges2 :: EdgeFilter
filterValidEdges2 edges = do
  (_, visitedEdges, visitedNodes) <- get
  let validEdges = do
        edge <- edges
        let crtNode = snd edge
        let crtCount = getCountOfCave visitedNodes crtNode
        let result | (isStart crtNode || isEnd crtNode) && crtCount >= 1 = []
                   | isSmallCave crtNode && crtCount > 1 = []
                   | isSmallCave crtNode && crtCount == 1 && isOtherWithTwoVisits visitedNodes crtNode = []
                   | otherwise = [edge]
        result
  return validEdges

isBigCave :: String -> Bool
isBigCave = all isUpper

isSmallCave :: String -> Bool
isSmallCave = all isLower

isEnd :: String -> Bool
isEnd "end" = True
isEnd _ = False

isStart :: String -> Bool
isStart "start" = True
isStart _ = False


type Node = String
type Graph = Map Node [Node]

addEdge :: Graph -> String -> String -> Graph
addEdge g a b = 
  let g1 = addEdgeDirected g a b
  in  addEdgeDirected g1 b a

addEdgeDirected :: Graph -> String -> String -> Graph
addEdgeDirected graph a b = 
  let existing = M.lookup a graph
      newGraph = case existing of 
        Just edges -> M.insert a (b:edges) graph
        Nothing -> M.insert a [b] graph
  in  newGraph

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

-- Notes:
-- 1. I was resetting the visited nodes bit of state when pushing the new edge.
--    I had to push the edge, but get the previous nodes instead of the initial
-- 2. I was also not removing the edges when getting back from recursion.  
-- 3. I was marking the current node as visited way to late, after finding the 
--    valid edges. This caused extra nodes to be added.
-- 4. From part1 to part2 I had to just change the data structure from set to map
--    and implement the conditions

