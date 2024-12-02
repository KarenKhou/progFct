module BinomialHeap where
import Criterion.Main
import GHC.Stats
import System.Mem

-- The typeclass for ordered elements, to be used by the heap
class Ord a => Element a where
    leq :: a -> a -> Bool

instance Element Int where
    leq = (<=)

-- Tree structure for Binomial Heap
data Tree a = Node Int a [Tree a]  -- Rank, Element, Child trees

-- Heap is a list of Tree elements
type Heap a = [Tree a]

-- Empty heap
empty :: Heap a
empty = []

-- Check if a heap is empty
isEmpty :: Heap a -> Bool
isEmpty = null

-- Rank of a tree
rank :: Tree a -> Int
rank (Node r _ _) = r

-- Root of a tree
root :: Tree a -> a
root (Node _ x _) = x

-- Linking two trees
link :: (Element a) => Tree a -> Tree a -> Tree a
link (Node r1 x1 c1) (Node r2 x2 c2)
    | leq x1 x2  = Node (r1 + 1) x1 (c2 ++ c1)
    | otherwise  = Node (r2 + 1) x2 (c1 ++ c2)


-- Insert a tree into the heap (maintains the heap property)
insTree :: (Element a) => Tree a -> Heap a -> Heap a
insTree t [] = [t]
insTree t (h:ts)
    | rank t < rank h = t : (h:ts)
    | otherwise       = insTree (link t h) ts

-- Insert an element into the heap
insert :: (Element a) => a -> Heap a -> Heap a
insert x ts = insTree (Node 0 x []) ts

-- Merging two heaps
merge :: (Element a) => Heap a -> Heap a -> Heap a
merge [] ts = ts
merge ts [] = ts
merge (h:hs) (t:ts)
    | rank h < rank t = h : merge hs (t:ts)
    | rank t < rank h = t : merge (h:hs) ts
    | otherwise       = insTree (link h t) (merge hs ts)

-- Remove the tree with the minimum root (i.e., minimum element)
removeMinTree :: (Element a) => Heap a -> (Tree a, Heap a)
removeMinTree [] = error "Empty heap"
removeMinTree [t] = (t, [])
removeMinTree (t:ts) = 
    let (t', ts') = removeMinTree ts
    in if leq (root t) (root t') then (t, ts) else (t', t:ts')

-- Find the minimum element of the heap
findMin :: (Element a) => Heap a -> a
findMin ts = root t
  where (t, _) = removeMinTree ts

-- Delete the minimum element of the heap
deleteMin :: (Element a) => Heap a -> Heap a
deleteMin [] = error "Empty heap"  -- Handle the empty heap case
deleteMin ts = merge (reverse te) te2
  where 
    (Node _ _ te, te2) = removeMinTree ts





-- Benchmarking function for inserting an element into the heap
benchmarkInsert :: Heap Int -> Benchmark
benchmarkInsert h = bench "insert" $ whnf (insert 42) h

-- Benchmarking function for merging two heaps
benchmarkMerge :: Heap Int -> Heap Int -> Benchmark
benchmarkMerge h1 h2 = bench "merge" $ whnf (merge h1) h2

-- Benchmarking function for deleting the minimum element
benchmarkDeleteMin :: Heap Int -> Benchmark
benchmarkDeleteMin h = bench "deleteMin" $ whnf deleteMin h

printMemoryUsage :: IO ()
printMemoryUsage = do
  performGC  -- Trigger garbage collection to get accurate stats
  stats <- getRTSStats
  putStrLn $ "Memory in use: " ++ show (gcdetails_mem_in_use_bytes $ gc stats) ++ " bytes"


main :: IO ()
main = do
  -- Run the benchmarks
  defaultMain [
      bgroup "BinomialHeap Operations" [
          benchmarkInsert empty,
          benchmarkMerge empty empty,
          benchmarkDeleteMin (insert 42 empty)  -- Insert an element before running deleteMin benchmark
        ]
    ]

  -- Print memory usage after benchmarks
  printMemoryUsage
