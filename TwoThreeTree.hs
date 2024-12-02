module Tree (Tree, empty, singleton, lookup, insert, fromList, toList) where
import Criterion.Main
import Prelude hiding (lookup)
import GHC.Stats
import System.Mem

-- Define your tree data structure and functions here


data Tree k v = Leaf 
              | Node2 (Tree k v) k v (Tree k v) 
              | Node3 (Tree k v) k v (Tree k v) k v (Tree k v) 
              deriving (Show, Read, Eq)

empty = Leaf

singleton :: k -> v -> Tree k v
singleton k v = Node2 Leaf k v Leaf

lookup :: (Ord k) => k -> Tree k v -> Maybe v
lookup k Leaf = Nothing
lookup k (Node2 lt k' v' rt) 
  | k < k' = Tree.lookup k lt
  | k == k' = Just v'
  | k > k' = Tree.lookup k rt
lookup k (Node3 lt k' v' mt k'' v'' rt) 
  | k < k'  = Tree.lookup k lt
  | k == k' = Just v'
  | k' < k && k < k'' = Tree.lookup k mt 
  | k == k'' = Just v''
  | k > k'' = Tree.lookup k rt

-- The `InsertionResult` is used in `add` function to push the information
-- about the result of insertion into a subtree up the stack. 
-- There are two possible results:
--
--   * Consumed t     - The elements were added to a subtree t and there
--                      is nothing to do at the upper level besides copying
--                      the path.
--   * Pushed l k v r - The elements were added down the tree and that
--                      forced us to divide the subtree into 2 subtrees
--                      that need to be inserted at an upper level.
data InsertionResult k v = Consumed (Tree k v) 
                         | Pushed (Tree k v) k v (Tree k v)

-- Insert an element (k, v) into the tree t, handling all possible
-- cases to preserve the balance.
insert :: (Ord k) => k -> v -> (Tree k v) -> (Tree k v)
insert k v t = 
  let add k v Leaf = Pushed Leaf k v Leaf
      {- First we handle all the corner cases, when a visited node is empty -}
      add k v (Node2 Leaf k' v' Leaf)
        | k < k'    = Consumed (Node3 Leaf k v Leaf k' v' Leaf)
        | k == k'   = Consumed (Node2 Leaf k v Leaf)
        | otherwise = Consumed (Node3 Leaf k' v' Leaf k v Leaf)
      add k v (Node3 Leaf k' v' Leaf k'' v'' Leaf)
        | k < k' = Pushed (singleton k v) k' v' (singleton k'' v'')
        | k == k' = Consumed (Node3 Leaf k v Leaf k'' v'' Leaf)
        | k' < k && k < k'' = Pushed (singleton k' v') k v (singleton k'' v'')
        | k == k'' = Consumed (Node3 Leaf k' v' Leaf k v Leaf)
        | otherwise = Pushed (singleton k' v') k'' v'' (singleton k v)
      {- Typical cases, when a visited node is full -}
      add k v (Node2 l k' v' r)
        | k < k'    = case add k v l of
          Consumed newL -> Consumed (Node2 newL k' v' r)
          Pushed newL k'' v'' newR -> Consumed (Node3 newL k'' v'' newR k' v' r)
        | k == k' = Consumed (Node2 l k v r)
        | otherwise = case add k v r of
          Consumed newR -> Consumed (Node2 l k' v' newR)
          Pushed newL k'' v'' newR -> Consumed (Node3 l k' v' newL k'' v'' newR)
      add k v (Node3 l k' v' m k'' v'' r)
        | k < k'        = case add k v l of
          Consumed newL -> Consumed (Node3 newL k' v' m k'' v'' r)
          Pushed newL x y newR -> Pushed (Node2 newL x y newR) k' v' (Node2 m k'' v'' r)
        | k == k' = Consumed (Node3 l k v m k'' v'' r)
        | k' < k && k < k'' = case add k v m of
          Consumed newM -> Consumed (Node3 l k' v' newM k'' v'' r)
          Pushed newL x y newR -> Pushed (Node2 l k' v' newL) x y (Node2 newR k'' v'' r)
        | k == k'' = Consumed (Node3 l k' v' m k v r)
        | otherwise     = case add k v r of
          Consumed newR -> Consumed (Node3 l k' v' m k'' v'' newR)
          Pushed newL x y newR -> Pushed (Node2 l k' v' m) k'' v'' (Node2 newL x y newR)
  {- If the two subtrees have been pushed whole way up the tree,
     we create a new Node2 root with these subtrees as children. -}
  in case add k v t of
    Consumed newT -> newT
    Pushed newL x y newR -> Node2 newL x y newR

              
fromList :: Ord k => [(k, v)] -> Tree k v
fromList [] = Leaf
fromList ((k, v):t) = insert k v (fromList t)

toList :: Tree k v -> [(k, v)]
toList Leaf = []
toList (Node2 lt k v rt) = (toList lt) ++ [(k,v)] ++ (toList rt)
toList (Node3 lt k v mt k' v' rt) = (toList lt) ++ [(k,v)]
                                    ++ (toList mt) ++ [(k',v')]
                                    ++ (toList rt)



-- Define the benchmark for `insert`
benchmarkInsert :: IO ()
benchmarkInsert = defaultMain [
    bgroup "insert"
      [ bench "small tree" $ whnf (insert 1 "a") (fromList [(2, "b"), (3, "c")])
      , bench "large tree" $ whnf (insert 10001 "new") (fromList [(i, show i) | i <- [1..10000]])
      ]
  ]

-- Define the benchmark for `lookup`
benchmarkLookup :: IO ()
benchmarkLookup = defaultMain [
    bgroup "lookup"
      [ bench "small tree" $ whnf (lookup 1) (fromList [(1, "a"), (2, "b"), (3, "c")])
      , bench "large tree" $ whnf (lookup 10000) (fromList [(i, show i) | i <- [1..10000]])
      ]
  ]

-- Define the benchmark for `fromList`
benchmarkFromList :: IO ()
benchmarkFromList = defaultMain [
    bgroup "fromList"
      [ bench "small list" $ whnf fromList [(1, "a"), (2, "b"), (3, "c")]
      , bench "large list" $ whnf fromList [(i, show i) | i <- [1..10000]]
      ]
  ]

-- Define the benchmark for `toList`
benchmarkToList :: IO ()
benchmarkToList = defaultMain [
    bgroup "toList"
      [ bench "small tree" $ whnf toList (fromList [(1, "a"), (2, "b"), (3, "c")])
      , bench "large tree" $ whnf toList (fromList [(i, show i) | i <- [1..10000]])
      ]
  ]

printMemoryUsage :: IO ()
printMemoryUsage = do
  performGC  -- Trigger garbage collection to get accurate stats
  stats <- getRTSStats
  putStrLn $ "Memory in use: " ++ show (gcdetails_mem_in_use_bytes $ gc stats) ++ " bytes"


-- Run all benchmarks
main :: IO ()
main = do
  benchmarkInsert
  benchmarkLookup
  benchmarkFromList
  benchmarkToList
  -- Print memory usage
  printMemoryUsage
