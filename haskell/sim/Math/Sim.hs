module Math.Sim where

import Prelude as P

import Math.Vector as V

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



-- -- force_between :: Particle -> Particle -> Force
force_between p1 p2 = let top = (mass p1 * mass p2) *> pos p1 <-> pos p2
                      in top
