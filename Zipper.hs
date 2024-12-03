import Criterion.Main
import GHC.Stats
import System.Mem

-- Binary tree data structure
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

-- Breadcrumbs for navigation in the tree
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
type Breadcrumbs a = [Crumb a]

-- Zipper type combining the current focus and breadcrumbs
type Zipper a = (Tree a, Breadcrumbs a)

-- Go to the left child of the current focus
goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node x left right, bs) = Just (left, LeftCrumb x right : bs)
goLeft (Empty, _) = Nothing

-- Go to the right child of the current focus
goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node x left right, bs) = Just (right, RightCrumb x left : bs)
goRight (Empty, _) = Nothing

-- Move back up to the parent node
goUp :: Zipper a -> Maybe (Zipper a)
goUp (_, []) = Nothing
goUp (t, LeftCrumb x right : bs) = Just (Node x t right, bs)
goUp (t, RightCrumb x left : bs) = Just (Node x left t, bs)

-- Modify the value at the current focus
modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x left right, bs) = (Node (f x) left right, bs)
modify _ (Empty, bs) = (Empty, bs)

-- Attach a new subtree at the current focus
attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

-- Start a zipper at the root of the tree
startZipper :: Tree a -> Zipper a
startZipper t = (t, [])


-- Define some sample trees to use for benchmarking
sampleTree :: Tree Int
sampleTree = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)

-- Define some benchmark functions
benchmarkGoLeft :: Zipper Int -> Benchmark
benchmarkGoLeft z = bench "goLeft" $ whnf goLeft z

benchmarkGoRight :: Zipper Int -> Benchmark
benchmarkGoRight z = bench "goRight" $ whnf goRight z

benchmarkGoUp :: Zipper Int -> Benchmark
benchmarkGoUp z = bench "goUp" $ whnf goUp z

benchmarkModify :: Zipper Int -> Benchmark
benchmarkModify z = bench "modify" $ whnf (modify (+1)) z

printMemoryUsage :: IO ()
printMemoryUsage = do
  performGC  -- Trigger garbage collection to get accurate stats
  stats <- getRTSStats
  putStrLn $ "Memory in use: " ++ show (gcdetails_mem_in_use_bytes $ gc stats) ++ " bytes"


main :: IO ()
main = do
  -- Run benchmarks
  defaultMain
    [ bgroup "Zipper Operations"
        [ benchmarkGoLeft (startZipper sampleTree)
        , benchmarkGoRight (startZipper sampleTree)
        , benchmarkGoUp (startZipper sampleTree)
        , benchmarkModify (startZipper sampleTree)
        ]
    ]

  -- Print memory usage
  printMemoryUsage
