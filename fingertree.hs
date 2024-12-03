
-- Define a Finger Tree type
data FingerTree a
  = Empty
  | Single a
  | Deep (FingerTree a) [a] (FingerTree a)

-- Access the first element
first :: FingerTree a -> Maybe a
first Empty = Nothing
first (Single x) = Just x
first (Deep _ (x:_) _) = Just x

-- Access the last element
last :: FingerTree a -> Maybe a
last Empty = Nothing
last (Single x) = Just x
last (Deep _ _ y) = last y

-- Constructing a finger tree from a list
fromList :: [a] -> FingerTree a
fromList [] = Empty
fromList [x] = Single x
fromList xs = Deep (fromList (take half xs)) (take half xs) (fromList (drop half xs))
  where half = length xs `div` 2

-- Helper to show the structure of the FingerTree (for debugging)
showFingerTree :: Show a => FingerTree a -> String
showFingerTree Empty = "Empty"
showFingerTree (Single x) = "Single " ++ show x
showFingerTree (Deep l xs r) = "Deep (" ++ showFingerTree l ++ ") " ++ show xs ++ " (" ++ showFingerTree r ++ ")"


-- Insert at the left end of the FingerTree
insertLeft :: a -> FingerTree a -> FingerTree a
insertLeft x Empty = Single x
insertLeft x (Single y) = Deep Empty [x] (Single y)
insertLeft x (Deep l xs r) = Deep (insertLeft x l) xs r

-- Insert at the right end of the FingerTree
insertRight :: a -> FingerTree a -> FingerTree a
insertRight x Empty = Single x
insertRight x (Single y) = Deep (Single y) [x] Empty
insertRight x (Deep l xs r) = Deep l xs (insertRight x r)



-- Example usage
main :: IO ()
main = do
  let ft = fromList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
  putStrLn $ "FingerTree: " ++ showFingerTree ft
  putStrLn $ "First element: " ++ show (first ft)
  putStrLn $ "Last element: " ++ show (last ft)

  let ft = fromList [1, 2, 3, 4, 5, 6]
  putStrLn $ "Original FingerTree: " ++ showFingerTree ft
  let ftLeft = insertLeft 0 ft
  putStrLn $ "After inserting 0 at the left: " ++ showFingerTree ftLeft
  let ftRight = insertRight 7 ft
  putStrLn $ "After inserting 7 at the right: " ++ showFingerTree ftRight
