import Data.List (map, filter, foldl', reverse)
import System.CPUTime (getCPUTime)
import Control.Exception (evaluate)
import GHC.Stats
import System.Mem

-- Helper function to convert time to nanoseconds
toNanoseconds :: Integer -> Double
toNanoseconds time = fromIntegral time / 1000000000

-- Benchmark functions
benchmark :: (Show a) => String -> ([a] -> b) -> [a] -> IO ()
benchmark name f lst = do
    start <- getCPUTime
    evaluate (f lst)  -- Evaluate to force computation before stopping the clock
    end <- getCPUTime
    let diff = end - start
    putStrLn $ name ++ ": " ++ show (toNanoseconds diff) ++ " seconds"

printMemoryUsage :: IO ()
printMemoryUsage = do
  performGC  -- Trigger garbage collection to get accurate stats
  stats <- getRTSStats
  putStrLn $ "Memory in use: " ++ show (gcdetails_mem_in_use_bytes $ gc stats) ++ " bytes"



-- Main function for benchmarking
main :: IO ()
main = do
    let smallList = [1, 2, 3]
    let largeList = [1..100000]

    -- Benchmark for small list
    putStrLn "Benchmark results for small list:"
    benchmark "map_list" (map (* 2)) smallList
    benchmark "filter_list" (filter even) smallList
    benchmark "fold_list" (foldl' (+) 0) smallList
    benchmark "reverse_list" reverse smallList

    -- Benchmark for large list
    putStrLn "\nBenchmark results for large list:"
    benchmark "map_list" (map (* 2)) largeList
    benchmark "filter_list" (filter even) largeList
    benchmark "fold_list" (foldl' (+) 0) largeList
    benchmark "reverse_list" reverse largeList

    -- Print memory usage after benchmarks
    printMemoryUsage
