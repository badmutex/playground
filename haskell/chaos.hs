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

data LabeledPoint = Label Int Int Point deriving (Eq, Show)

die dim = randomRIO (1, dim)

midPoint (x1,y1) (x2,y2) = ( (x1+x2) / 2,
                             (y1+y2) / 2)

choose i xs = find (\(Label j k p) -> i == j || i == k) xs

data Num a => RealRatio a = !a :/: !a deriving Show

divRate i n = s1 :/: s2 where
    sinval = sin (2*pi*i/n)
    s1 = sinval / (1 + sinval)
    s2 = 1 / (1 + sinval)


vertex n i = let v = 2 * pi * i / n
                 x = cos v
                 y = sin v
             in (x, y)


mthPoint :: Float -> Float -> State Point Point
mthPoint n i = let s@(s1 :/: s2) = divRate i n
                   v@(x,y) = vertex n i
                   compute v p = (v - p) * s2 + p
               in do
                 prev@(xp, yp) <- get
                 let point = (compute x xp, compute y yp)
                 put point
                 return $ point -- uncurry Translate point (Circle 0.4)


loop :: (Float -> State Point Point) -> Float -> Float -> State Point [Point]
loop f i max = do if i >= max
                     then return []
                     else do p <- f i
                             liftM (p:) $ loop f (i+1) max

chaos n max = evalState (loop (mthPoint n) 0 max) (0,0)

main = do
  let pics = chaos 3 10000
  displayInWindow "Chaos Rules!" (1650, 900) (0,0) white (Line pics)
