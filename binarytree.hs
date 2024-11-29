import GHC.Stats
import System.Mem

-- Binary Tree Data Structure
data BinaryTree a = Empty
                  | Node a (BinaryTree a) (BinaryTree a)
                  deriving (Show, Eq)

-- Insert an element into the binary search tree
insert :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insert x Empty = Node x Empty Empty
insert x (Node v left right)
  | x < v     = Node v (insert x left) right
  | x > v     = Node v left (insert x right)
  | otherwise = Node v left right  -- No duplicates allowed

  -- Lookup an element in the binary search tree
lookup :: (Ord a) => a -> BinaryTree a -> Bool
lookup _ Empty = False
lookup x (Node v left right)
  | x < v     = lookup x left
  | x > v     = lookup x right
  | otherwise = True


-- In-order traversal
inOrder :: BinaryTree a -> [a]
inOrder Empty = []
inOrder (Node v left right) = inOrder left ++ [v] ++ inOrder right

-- Pre-order traversal
preOrder :: BinaryTree a -> [a]
preOrder Empty = []
preOrder (Node v left right) = [v] ++ preOrder left ++ preOrder right

-- Post-order traversal
postOrder :: BinaryTree a -> [a]
postOrder Empty = []
postOrder (Node v left right) = postOrder left ++ postOrder right ++ [v]


printMemoryUsage :: IO ()
printMemoryUsage = do
  performGC  -- Trigger garbage collection to get accurate stats
  stats <- getRTSStats
  putStrLn $ "Memory in use: " ++ show (gcdetails_mem_in_use_bytes $ gc stats) ++ " bytes"


main :: IO ()
main = do
  let tree = Empty
  let tree1 = insert 10 tree
  let tree2 = insert 5 tree1
  let tree3 = insert 15 tree2
  let tree4 = insert 7 tree3
  let tree5 = insert 12 tree4

  putStrLn "In-order traversal (sorted):"
  print $ inOrder tree5

  putStrLn "\nPre-order traversal:"
  print $ preOrder tree5

  putStrLn "\nPost-order traversal:"
  print $ postOrder tree5

  -- Print memory usage
  printMemoryUsage
