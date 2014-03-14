module BinomialHeap where

import Data.List (sort)
import System.Random
import Test.QuickCheck

-- | Binomial tree
data Tree a = Node { rank :: Int
                   , root :: a
                   , children :: Heap a
                   } deriving (Show)

instance (Ord a, Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = choose (1, 8) >>= randTree

randTree :: (Ord a, Arbitrary a) => Int -> Gen (Tree a)
randTree 1 = do val <- arbitrary
                return $ Node 1 val empty
randTree n = do t1 <- randTree (n-1)
                t2 <- randTree (n-1)
                return $ link t1 t2

-- | A heap is a list of trees ordered by rank.
type Heap a = [Tree a]

empty :: Heap a
empty = []

isEmpty :: Heap a -> Bool
isEmpty = (==0). length

insert :: Ord a => a -> Heap a -> Heap a
insert x ts = insertTree t ts
              where t = Node 1 x empty

merge :: Ord a => Heap a -> Heap a -> Heap a
merge [] h2 = h2
merge h1 [] = h1
merge h1@(t1:ts1) h2@(t2:ts2)
  | rank t1 < rank t2 = t1 : (merge ts1 h2)
  | rank t1 > rank t2 = t2 : (merge h1 ts2)
  | otherwise = insertTree (link t1 t2) (merge ts1 ts2)

findMin :: Ord a => Heap a -> Maybe a
findMin [] = Nothing
findMin (t:ts) = Just $ foldl min (root t) (map root ts)

deleteMin :: Ord a => Heap a -> Heap a
deleteMin [] = []
deleteMin ts = let (t', ts') = getMin ts
                   ts'' = (reverse. children) t'
               in merge ts' ts''
  where getMin (x:xs) = foldl cmp (x, []) xs
        cmp (x, xs) y = if root x <= root y then (x, y:xs)
                        else (y, x:xs)

-- helper function
-- | Combine two binominal trees
link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Node r1 x1 ts1) t2@(Node r2 x2 ts2) =
  if x1 <= x2
  then Node (r1 + 1) x1 (t2 : ts1)
  else Node (r2 + 1) x2 (t1 : ts1)

-- | Insert a tree into a heap
insertTree :: Ord a => Tree a -> Heap a -> Heap a
insertTree x [] = [x]
insertTree x (t:ts) = if rank x < rank t
                      then x:t:ts
                      else insertTree (link x t) ts


-- | Tests
prop_rootIsMinimal :: (Ord a) => Tree a -> Bool
prop_rootIsMinimal t = foldl min (root t) (map root (children t)) == root t

prop_insertPreserves :: (Ord a) => [a] -> Property
prop_insertPreserves xs = not (null xs) ==>
  head xs' == m
  where xs' = sort xs
        h = foldl (flip insert) empty xs
        (Just m) = findMin h
