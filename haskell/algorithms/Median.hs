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

isort v
      | V.null v  = V.empty
      | otherwise = let (h,t) = (V.head v, V.tail v)
                    in insertBy (<=) h (isort t)




v :: V.Vector Int
v = V.fromList . reverse $ [1..10]
