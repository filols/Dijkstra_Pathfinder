module Skewheap(SkewHeap, 
                insert, delete, emptyHeap, isEmpty, heapHead, pop, toList, removeLowest) where

-- Data
data SkewHeap a = Empty
                | Node a (SkewHeap a) (SkewHeap a)
                deriving Show

-- Deletes input from the heap if it exists, retains invariant
-- O(n log n)
delete :: Ord a => a -> SkewHeap a -> SkewHeap a
delete y Empty = Empty
delete y h @(Node x left right)
    | y < x     = h
    | y == x    = left `merge` right
    | otherwise = Node x (delete y left) (delete y right)

-- Removes the smallest element from the heap, retains invariant
-- O(log n)
removeLowest :: Ord a => SkewHeap a -> SkewHeap a
removeLowest Empty               = Empty
removeLowest (Node x left right) = left `merge` right

-- Recursively checks the lowest value between the two nodes and merge accordingly to maintain 
-- skew heap invariant
-- Amortised O(log n) complexity
merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
merge h1 Empty = h1
merge Empty h2 = h2
merge h1 @(Node a heap1Left heap1Right) h2 @(Node b heap2Left heap2Right)
    | a < b     = Node a (merge heap1Right h2) heap1Left
    | otherwise = Node b (merge heap2Right h1) heap2Left

-- Inserts and retains invariant
-- O(log n)
insert :: Ord a => a -> SkewHeap a -> SkewHeap a
insert a = merge (Node a Empty Empty)

-- Returns an empty heap
-- O(1)
emptyHeap :: Ord a => SkewHeap a
emptyHeap = Empty

-- Evaluates if heap is empty
-- O(1)
isEmpty :: Ord a => SkewHeap a -> Bool
isEmpty (Node a _ _) = False
isEmpty _            = True

-- Heap conversion to list
-- O(n log n)
toList :: Ord a => SkewHeap a -> [a]
toList Empty = []
toList (Node a left right) = a : toList (left `merge` right)

-- Returns the element on top of the heap, i.e. the lowest (by ordering).
-- O(1)
heapHead :: Ord a => SkewHeap a -> Maybe a
heapHead Empty        = Nothing
heapHead (Node x _ _) = Just x

-- Returns the element on top of the heap and the heap with said element removed.
-- O(log n)
pop :: Ord a => SkewHeap a -> Maybe (a, SkewHeap a)
pop Empty        = Nothing
pop heap @(Node x _ _) = Just (x, removeLowest heap)