-- | linear-time algorithm to find the median.
--
-- uses prune and search paradigm.
-- see http://www.cs.duke.edu/courses/fall05/cps230/L-03.pdf for details
--
-- 1. partition n items in ceiling(n/5) groups of size at most 5 each
-- 2. find the median in each group
-- 3. find the median of the medians recursively
-- 5. split the array using the median of the medians as the pivot
-- 6. recurse on the pivot

module Median where

import Debug.Trace
import Text.Printf

import qualified Data.Vector.Unboxed as V
import Control.Applicative
import Control.Monad
import Control.Monad.State



partition :: V.Unbox a => V.Vector a -> [V.Vector a]
partition v = let spans     = [0,5..V.length v - 1]
                  catch i j = if i + j >= V.length v - 1
                              then (i, V.length v - i)
                              else (i, j)
                  slicer i  = uncurry V.slice (catch i 5) v
              in map slicer spans



type ArrayState a = State (V.Vector a)

swap :: V.Unbox a => Int -> Int -> ArrayState a ()
swap i j = do
  v <- get
  let iv = v V.! i
      jv = v V.! j
      v' = v V.// [(i,jv), (j,iv)]
  put v'


while g m = do
  p <- g
  if p
    then do m
            while g m
    else return ()







tw :: State Int ()
tw = do while
             ( (<5) <$> get )
             ( (+1) <$> get >>= put )


v :: V.Vector Int
v = V.fromList $ reverse [0..19]

t = mapM_ print $ partition v