{-# LANGUAGE TemplateHaskell #-}

-- | Amortized queue, from Chris Okasaki, Purely Functional Data Structures
module Queue (
  -- * Queue type
  Queue

  -- * Creation/Inserting operations
  , emptyQueue
  , snoc
  , enqueue

  -- * Inspections
  , isEmpty
  , qHead
  , qTail
  , qSize

  -- * Conversion to other data structure
  , toList

  -- * Testing
  , runTests
  ) where

import Test.QuickCheck
import Test.QuickCheck.All

data Queue a = Q { f :: [a], r :: [a] } deriving (Show)

instance (Arbitrary a) => Arbitrary (Queue a) where
  arbitrary = do xs <- arbitrary
                 return $ enqueue emptyQueue xs

-- Constructor helper, ensure invariant len(f) >= len(r)
queue :: Queue a -> Queue a
queue q@(Q f r)
  | length f < length r = Q (f ++ reverse r) []
  | otherwise = q

-- | Create empty queue
emptyQueue :: Queue a
emptyQueue = Q [] []

-- | Add single item
snoc :: Queue a -> a -> Queue a
snoc (Q f r) x = queue $ Q f (x:r)

-- | Add multiple items
enqueue :: Queue a -> [a] -> Queue a
enqueue = foldl snoc

-- | Check if queue is empty
isEmpty :: Queue a -> Bool
isEmpty = null . f

-- | Get first element of queue
qHead :: Queue a -> a
qHead (Q [] _) = error "qHead: empty queue."
qHead (Q (x:xs) _) = x

-- | Get all but first element in queue
qTail :: Queue a -> Queue a
qTail q@(Q [] _) = error "qTail: empty queue"
qTail (Q (x:xs) r) = queue $ Q xs r

-- | Queue length
qSize :: Queue a -> Int
qSize (Q f r) = length f + length r

-- | Convert to list
toList :: Queue a -> [a]
toList (Q f r) = f ++ reverse r

-- test properties
prop_queueFrontIsLongerThanRear q =
  length (f q) >= length (r q)
prop_queuePreservesInsertionOrder xs =
  toList (enqueue emptyQueue xs) == xs
prop_queueHeadPreservesOrder q =
  not (isEmpty q) ==> qHead q == (head . toList) q
prop_queueTailPreservesOrder q =
  not (isEmpty q) ==> (toList . qTail) q == (tail . toList) q
prop_queueSize q =
  qSize q == (length . toList) q
prop_queueHeadPreservesOverInsertion q x =
  not (isEmpty q) ==> qHead q == qHead (snoc q x)

-- | QuickCheckAll
runTests = $quickCheckAll
