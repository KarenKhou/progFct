module Main where
import Control.DeepSeq (NFData(..), deepseq)
import Criterion.Main
import System.Mem (performGC)
import GHC.Stats
import System.Mem

-- Custom List Definition
data List a = Empty | Cons a (List a) deriving (Show, Eq)

-- Making List an instance of NFData
instance NFData a => NFData (List a) where
  rnf Empty = ()
  rnf (Cons x xs) = x `deepseq` rnf xs

-- Mapping a function over List
mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Cons x xs) = Cons (f x) (mapList f xs)

-- Filtering elements from List
filterList :: (a -> Bool) -> List a -> List a
filterList _ Empty = Empty
filterList p (Cons x xs)
  | p x       = Cons x (filterList p xs)
  | otherwise = filterList p xs

-- Reducing List to a single value
foldList :: (b -> a -> b) -> b -> List a -> b
foldList _ acc Empty = acc
foldList f acc (Cons x xs) = foldList f (f acc x) xs

-- Reversing the List
reverseList :: List a -> List a
reverseList = foldList (flip Cons) Empty

-- Appending two Lists
append :: List a -> List a -> List a
append Empty ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

-- Inserting an element at a specific position in the List
insert :: Int -> a -> List a -> List a
insert _ x Empty = Cons x Empty  -- Insert at any position in an empty list
insert 0 x xs = Cons x xs        -- Insert at the head of the list
insert n x (Cons y ys)
  | n > 0     = Cons y (insert (n - 1) x ys)  -- Recur, decrementing the position
  | otherwise = Cons y ys  -- Position invalid, return the original list unchanged

-- Lookup function to find an element in List
listLookup :: Eq a => a -> List a -> Bool
listLookup _ Empty = False
listLookup x (Cons y ys)
  | x == y    = True
  | otherwise = listLookup x ys

-- Converting a Haskell List to Custom List
toList :: [a] -> List a
toList = foldr Cons Empty

-- Converting a Custom List to Haskell List
fromList :: List a -> [a]
fromList Empty = []
fromList (Cons x xs) = x : fromList xs

printMemoryUsage :: IO ()
printMemoryUsage = do
  performGC  -- Trigger garbage collection to get accurate stats
  stats <- getRTSStats
  putStrLn $ "Memory in use: " ++ show (gcdetails_mem_in_use_bytes $ gc stats) ++ " bytes"

main :: IO ()
main = do
  -- Explicit Type Annotations
  let smallList :: List Int
      smallList = Cons 1 (Cons 2 (Cons 3 Empty))

      largeList :: List Int
      largeList = toList [1..100000]

  -- Force Garbage Collection
  performGC

  -- Enable memory profiling
  putStrLn "Starting Benchmark with Memory Profiling..."

  -- Run benchmarks
  defaultMain
    [ bgroup "mapList"
        [ bench "smallList" $ nf (mapList (*2)) smallList
        , bench "largeList" $ nf (mapList (*2)) largeList
        ]
    , bgroup "filterList"
        [ bench "smallList" $ nf (filterList even) smallList
        , bench "largeList" $ nf (filterList even) largeList
        ]
    , bgroup "foldList"
        [ bench "smallList" $ nf (foldList (+) 0) smallList
        , bench "largeList" $ nf (foldList (+) 0) largeList
        ]
    , bgroup "reverseList"
        [ bench "smallList" $ nf reverseList smallList
        , bench "largeList" $ nf reverseList largeList
        ]
    , bgroup "append"
        [ bench "small + small" $ nf (append smallList) smallList
        , bench "large + small" $ nf (append largeList) smallList
        ]
    , bgroup "insert"
        [ bench "insert at start" $ nf (insert 0 100) smallList
        , bench "insert at middle" $ nf (insert 1 100) smallList
        , bench "insert at end" $ nf (insert 3 100) smallList
        ]
    , bgroup "listLookup"
        [ bench "lookup 2 in smallList" $ nf (listLookup 2) smallList
        , bench "lookup 100000 in largeList" $ nf (listLookup 100000) largeList
        , bench "lookup 50000 in largeList" $ nf (listLookup 50000) largeList
        ]
    ]
  -- Print memory usage after benchmarks
  printMemoryUsage
