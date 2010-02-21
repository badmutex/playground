-- | linear-time algorithm to find the median.
--
-- uses prune and search paradigm.
-- see
-- http://www.cs.duke.edu/courses/fall05/cps230/L-03.pdf and
-- http://par.cse.nsysu.edu.tw/~cbyang/course/algo/algonote/algo6.ppt for details
--
-- 1. partition n items in ceiling(n/5) groups of size at most 5 each
-- 2. find the median in each group
-- 3. find the median of the medians recursively
-- 5. split the array using the median of the medians as the pivot
-- 6. recurse on the pivot
--
-- 

module Median where

import Debug.Trace
import Text.Printf

import qualified Data.Vector as V
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.List (sort)
import Data.Ord



insertBy :: (a -> a -> Bool) -> a -> V.Vector a -> V.Vector a
insertBy f a v
    | V.null v  = V.singleton a
    | otherwise = let h = V.head v
                  in if f a h
                     then V.cons h $ insertBy f a (V.tail v)
                     else V.cons a v

isort :: Ord a => V.Vector a -> V.Vector a
isort v
      | V.null v  = V.empty
      | otherwise = let (h,t) = (V.head v, V.tail v)
                    in insertBy (<=) h (isort t)

sortedMedian :: Ord a => V.Vector a -> a
sortedMedian v = s V.! m
    where s = isort v
          m = V.length v `div` 2


safeSlice :: Int -> Int -> V.Vector a -> V.Vector a
safeSlice i j v = uncurry V.slice range v
    where range = if i + j >= V.length v
                  then (i, V.length v - i)
                  else (i, j)

binsOf5 :: V.Vector a -> V.Vector (V.Vector a)
binsOf5 v = let spans = V.fromList [0,5..V.length v - 1]
                slicer i = safeSlice i 5 v
            in V.map slicer spans

pivot :: Ord a => V.Vector a -> a
pivot v = let medians = V.map isort . binsOf5 $ v
          in  sortedMedian $ V.map sortedMedian medians

partitionAround :: Ord a => a -> V.Vector a -> [V.Vector a]
partitionAround p v = let (le, gte) = V.unstablePartition (< p) v
                          (eq, gt ) = V.unstablePartition (==p) gte
                      in [le, eq, gt]

data Location = LeftSide | Pivot | RightSide deriving Show

decide :: Int -> [Int] -> Location
decide k s@[s1, s2, s3]
    | s1 >= k       = LeftSide
    | s1 + s3 == k  = Pivot
    | otherwise     = RightSide


kthSmallest :: Ord a => Int -> V.Vector a -> a
kthSmallest k v
    | V.null v = error "no solution"
    | otherwise =
        let p = pivot v
            subsets@[lt, eq, gt] = partitionAround p v
            sizes@[s1, s2, s3]   = map V.length subsets
        in case decide k sizes of
             Pivot     -> p
             LeftSide  -> kthSmallest k lt
             RightSide -> let k' = let tot   = sum sizes
                                       rsize = tot - (s1 + s2)
                                       ksize = tot - k
                                   in rsize - ksize
                          in kthSmallest k' gt


v :: V.Vector Int
v = V.fromList . reverse $ [1..20]
