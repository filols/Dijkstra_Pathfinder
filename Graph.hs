module Graph(Edge, Graph, 
            empty, vertices, addEdge, constructEdge, edgeDst, edgeSource, edgeLabel, 
            addDistance, findEdges) where
import Data.Map (Map)
import qualified Data.Map as M

-- | Data
data Edge a b = Edge
  { source :: a
  , label  :: b
  , dst    :: a
  } deriving (Show, Eq)

instance (Eq a, Ord b) => Ord (Edge a b) where
  (Edge _ label1 _) > (Edge _ label2 _)  = label1 > label2
  (Edge _ label1 _) <= (Edge _ label2 _) = label1 <= label2

data Graph a b = Graph
  { adjMap :: Map a [Edge a b]
  } deriving Show

-- Returns an empty graph
-- O(1)
empty :: Graph a b
empty = Graph M.empty

-- Retruns vertices
-- O(n)
vertices :: Graph a b -> [a]
vertices = M.keys . adjMap

-- Adds edge to graph
-- O(log n)
addEdge :: Ord a => Edge a b -> Graph a b -> Graph a b
addEdge e@(Edge s _ _) g@(Graph a) = Graph (M.insertWith (++) s [e] a)

-- Returns an edge based on input
-- O(1)
constructEdge :: Ord a => a -> b -> a -> Edge a b
constructEdge = Edge

-- Returns source of an edge
-- O(1)
edgeSource :: Edge a b -> a
edgeSource (Edge s _ _) = s

-- Returns label of an edge
-- O(1)
edgeLabel :: Edge a b -> b
edgeLabel (Edge _ l _) = l

-- Returns dst of an edge
-- O(1)
edgeDst :: Edge a b -> a
edgeDst (Edge _ _ d) = d

-- Takes a key and a graph and returns the list of edges for the given key value, returns 
-- Nothing if key does not exist.
-- O(log n)
findEdges :: Ord a => a -> Graph a b -> Maybe [Edge a b] 
findEdges a (Graph adjMap) = M.lookup a adjMap

-- Adds 
-- O(1)
addDistance :: Num b => b -> Edge a b -> Edge a b
addDistance label (Edge src l dst) = Edge src (label + l) dst