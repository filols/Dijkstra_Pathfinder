import Route
import RouteGUI
import Graph  -- Create a module and use a sensible graph representation
import Skewheap (emptyHeap, insert, pop, removeLowest, SkewHeap)
import System.Environment (getArgs)
import Data.Map (Map)
import qualified Data.Map as M

-- Checks whether the destination is the same as src. If not proceeds to do djikstra and 
-- find the shortest path (if it exists). 
-- O(n log n) where n is the amount of edges in the graph
shortestPath :: Graph Name Cost -> Name -> Name -> Maybe ([Name], Cost)
shortestPath graph src dst
  | src == dst = Just ([src], 0)
  | otherwise  = case M.lookup dst set of
      Nothing   -> Nothing
      Just edge -> Just (backtrace set src edge, edgeLabel edge)
    where
      set = dijkstra graph initialHeap (M.insert src (constructEdge src 0 src) M.empty)
      initialHeap = addEdges (findEdges src graph) emptyHeap 0

-- | Recursively takes the current smallest element in the heap. If this element has not been 
-- visited before the its edges will be added to the heap and the element will be added to the set 
-- of already visited nodes with a label corresponding to the distance from the source node. The key
-- in the set is equal to the destination node. Returns the set of nodes once the heap is exhausted.  
-- O(n log n) where n is the amount of edges in the graph
dijkstra :: (Ord a, Ord b, Num b) =>
 Graph a b -> SkewHeap (Edge a b) -> Map a (Edge a b) -> Map a (Edge a b)
dijkstra graph heap set = case pop heap of
  Nothing            -> set
  Just (edge, heap') -> if M.member edgeDst' set 
    then dijkstra graph heap' set 
    else dijkstra graph (addEdges edges heap' cost) (M.insert edgeDst' edge set)
      where
        edgeDst'  = edgeDst edge
        cost      = edgeLabel edge
        edges     = findEdges (edgeDst edge) graph

-- Takes a list of edges, a heap, and a cost. Adds the cost to every edge in the list then adds 
-- every (updated) edge to the heap. 
-- O(n log n) where n is the amount of edges in the list
addEdges :: (Ord a, Ord b, Num b) =>
 Maybe [Edge a b] -> SkewHeap (Edge a b) -> b -> SkewHeap (Edge a b)
addEdges Nothing      heap cost = heap
addEdges (Just edges) heap cost =
  foldl (\heap x -> insert (addDistance cost x) heap) heap edges

-- | Backtraces from an edge to a given source. Reverses to return the list ordered from source to 
-- destination. 
-- O(n log n) where n is the amount of keys in the map
backtrace :: Ord a => Map a (Edge a b) -> a -> Edge a b -> [a]
backtrace set src edge = reverse $ backtrace' set src edge

-- Recursively backtraces to a given source, adding every stop along the way.
-- O(n log n) where n is the amount of keys in the map
backtrace' :: Ord a => Map a (Edge a b) -> a -> Edge a b -> [a]
backtrace' set src edge
  | edgeSource edge == src = [dst, src]
  | otherwise              = dst : backtrace' set src edge'
    where
      dst   = edgeDst edge
      edge' = set M.! edgeSource edge

-- | For every LineTable parseLines runs parseStop which in turn parses every edge, returns the
-- resulting graph. As every LineTable has at least one stop it follows that: 
-- O(stops) >= O(linetables) => O(linetables) + O(stops) = O(stops)
-- O(n) where n is amount of stops
parseLines :: [LineTable] -> Graph String Integer -> Graph String Integer
parseLines []                               graph = graph
parseLines (LineTable _ stops : lineTables) graph = parseLines lineTables (parseStop stops graph)

-- Traverses a list of stops and adds a directed edge between a given stop and the next stop in line
-- O(n) where n is amount of stops
parseStop :: [LineStop] -> Graph String Integer -> Graph String Integer
parseStop []        graph = graph
parseStop lineStops graph = if length lineStops' > 0 
  then parseStop lineStops' (parseStop' (head lineStops) (head lineStops') graph)
  else graph
  where
      lineStops' = drop 1 lineStops

-- Adds an edge between two stops
-- O(1)
parseStop' :: LineStop -> LineStop -> Graph String Integer -> Graph String Integer
parseStop' (LineStop src _) (LineStop dst cost)
  = addEdge (constructEdge src cost dst)

-- | Main
-- Calculates and prints shortest path's cost and node traversal
main :: IO ()
main = do
  args <- getArgs
  --if length args /= 4 then do
  --  putStrLn "Faulty input, enter arguments <stopsFile> <linesFile> <source> <destination>"
  --  return ()
  --else do
  let stops = head args
  let lines = args !! 1
  let src   = args !! 2
  let dst   = last args
  
  Right stoplines <- readLines lines
  case shortestPath (parseLines stoplines empty) src dst of
    Just (names, time) -> do
      print time
      putStr $ unlines names
    _                  -> putStrLn "No path!"

-- | startGUI
startGUI :: IO ()
startGUI = do
  Right stops <- readStops "stops-gbg.txt"
  Right lines <- readLines "lines-gbg.txt"
  let graph = parseLines lines empty
  runGUI stops lines graph shortestPath