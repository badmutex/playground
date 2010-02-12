-- | implementation of convex hull algorithm using prune and search

import Control.Monad.State
import Data.List
import Data.Ord

data Point = P {xval, yval :: Double} deriving (Eq, Ord, Read, Show)

medianPoint :: [Point] -> Point
medianPoint = medianBy $ comparing xval

-- | TODO implement using linear-time prune and search
medianBy :: (a -> a -> Ordering) -> [a] -> a
medianBy f xs = let s = sortBy f xs
                in s !! (length xs `div` 2)



type PointsAvailable = State [Point] [Point]

prunePoints :: Point -> [Point] -> [Point]
prunePoints median = filter (\p -> xval p <= xval median || xval p >= xval median)

searchPoints :: [Point] -> [Point]
searchPoints ps = let median = medianPoint ps
                  in undefined


-- | pairwise associations
assocs :: [a] -> [(a,a)]
assocs xs = zip xs (tail xs ++ [head xs])

runAlg :: State [Point] ()
runAlg = do
  ps <- get
  put $ tail ps

convexHull :: State [Point] [Point]
convexHull = do
  runAlg
  get
