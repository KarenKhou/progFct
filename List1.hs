module Main where
import Control.DeepSeq (NFData(..), deepseq)
import Criterion.Main
import System.Mem (performGC)


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

-- Converting a Haskell List to Custom List
toList :: [a] -> List a
toList = foldr Cons Empty

-- Converting a Custom List to Haskell List
fromList :: List a -> [a]
fromList Empty = []
fromList (Cons x xs) = x : fromList xs

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
    ]


