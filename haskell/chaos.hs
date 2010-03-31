{-# LANGUAGE NoMonomorphismRestriction #-}

import Graphics.Gloss
import System.Random
import Control.Monad
import Control.Monad.State
import Data.List


newPoint :: (Num a, Random a) => (a,a) -> IO (a,a)
newPoint r = do
  x <- randomRIO r
  y <- randomRIO r
  return (x,y)

size = (1600,900)
range = (0,900)

type MyState = StateT (Point, [LabeledPoint]) IO

data LabeledPoint = Label Int Point deriving (Eq, Show)

die dim = randomRIO (1, dim)

midPoint (x1,y1) (x2,y2) = ( (x1+x2) / 2,
                             (y1+y2) / 2)

choose i xs = find (\(Label j p) -> i == j) xs


vertices dim range = do
  ps <- replicateM dim $ newPoint range
  return $ zipWith (\i p -> Label i p) [1..] ps

nextPoint :: Int -> (Float, Float) -> MyState Picture
nextPoint dim range = do
  (prev, bounds) <- get
  label <- liftIO $ die dim
  let Just (Label _ vertex) = choose label bounds
      next@(x,y) = midPoint prev vertex
      state = (next, bounds)
  put state
  return $ Translate x y (Circle 0.4)


loop :: Int -> Int -> Point -> MyState [Picture]
loop 0 _ _ = return []
loop n dim range = do
  p <- nextPoint dim range
  liftM (p :) (loop (n-1) dim range)


chaos :: Int -> Int -> IO [Picture]
chaos dim reps = do
  seed <- newPoint range
  bounds <- vertices dim range
  evalStateT (loop reps dim range) (seed, bounds)



main = do
  pics <- chaos 3 50000
  displayInWindow "hello world" size (0,0) white (Color red (Pictures pics))
