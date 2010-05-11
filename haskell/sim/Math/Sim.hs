

import Data.Vector as V
import Prelude as P

import Math.Vector

type Position = Tagged D3 (Vector Double)
type Velocity = Tagged D3 (Vector Double)
type Mass     = Double
type Force    = Double
type Index    = Int

data Particle = P { pos  :: !Position
                  , vel  :: !Velocity
                  , mass :: !Mass
                  }

type Particles = Vector Particle


data SimState = S { particles :: Particles }


class Vec a n where
    (<->) :: a -> a -> a
    (*>)  :: n -> a -> a


instance Vec Position Double where
    (<->) = vecOp (-)
    (*>)  = scalarMul



vecOp :: (a -> b -> c) -> Tagged t (Vector a) -> Tagged t (Vector b) -> Tagged t (Vector c)
vecOp op a b = let (x,y) = (untag a, untag b)
               in T $ V.zipWith op x y

-- scalarMul :: Num n => n -> Tagged t (Vector n) -> Tagged t (Vector n)
-- scalarMul a v = vecOp (*) (T $ V.replicate 3 a) v


-- (<->) = vecOp (-)
-- (*>)  = scalarMul

-- force_between :: Particle -> Particle -> Force
force_between p1 p2 = let top = (mass p1 * mass p2) *> pos p1 <-> pos p2

                      in top