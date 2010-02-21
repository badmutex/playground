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



groups :: Ord a => V.Vector a -> V.Vector (V.Vector a)
groups v = let spans     = V.fromList [0,5..V.length v - 1]
               catch i j = if i + j >= V.length v - 1
                           then (i, V.length v - i)
                           else (i, j)
               slicer i  = uncurry V.slice (catch i 5) v
           in V.map slicer spans

-- partition :: Ord a => V.Vector a -> a -> (V.Vector a, V.Vector a, V.Vector a)
partition v p = let (lt, gte) = V.partition (< p) v
                    (eq, gt ) = V.partition (==p) gte
                in trace (printf "[partition] v %s p %s" (show v) (show p))
                         (lt, eq, gt)


msort :: Ord a => V.Vector a -> V.Vector a
msort = V.fromList . sort . V.toList


insertBy :: (a -> a -> Bool) -> a -> V.Vector a -> V.Vector a
insertBy f a v
    | V.null v  = V.singleton a
    | otherwise = let h = V.head v
                  in if f a h
                     then V.cons h $ insertBy f a (V.tail v)
                     else V.cons a v

isort v
      | V.null v  = V.empty
      | otherwise = let (h,t) = (V.head v, V.tail v)
                    in insertBy (<=) h (isort t)

groupMedian :: Ord a => V.Vector a -> a
groupMedian v = let s = msort v
                in s V.! ((V.length v - 1) `div` 2)

sortedMedian :: Ord a => V.Vector a -> a
sortedMedian v = (V.! (V.length v `div` 2)) $ msort v


sortedMedians :: Ord a => V.Vector a -> V.Vector a
sortedMedians = V.map sortedMedian . groups


pivot v = let p' = sortedMedian . sortedMedians $ v
          in trace (printf "[pivot] v %s p' %s" (show v) (show p'))
                   p'


mpos v = V.length v `div` 2


kth vs@(lt,eq,gt) p k
    | V.length lt >= k                = let p' = trace (printf "[kth] lt = %s k = %d" (show lt) k)
                                                       pivot lt
                                        in kth (partition lt p') p' k
    | V.length lt + V.length eq >= k  = trace (printf "[kth] done: %d" p)
                                             p
    | otherwise                       = let p' = pivot gt
                                            k' = k - V.length lt - V.length gt
                                        in trace (printf "[kth] p' %s k' %d" (show p') k')
                                                 kth (partition gt p') p' k'

median v = let p = pivot v
               k = V.length v `div` 2
           in kth (partition v p) p k


v :: V.Vector Int
v = V.fromList . reverse $ [1..10]