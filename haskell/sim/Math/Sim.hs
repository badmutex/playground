module Math.Sim where

import Prelude as P

import Math.Vector

import Data.Vector as V


type Position = Vec D3 Vector Double
type Velocity = Vec D3 Vector Double
type Force    = Vec D3 Vector Double
type Mass     = Double
type Index    = Int

data Particle = P { pos  :: !Position
                  , vel  :: !Velocity
                  , mass :: !Mass
                  }
              deriving Show

type Particles = Vector Particle


data SimState = S { particles :: Particles }



force_between :: Particle -> Particle -> Force
force_between p1 p2 = let top = (mass p1 * mass p2) *> (pos p1 <-> pos p2)
                          bot = norm (pos p1 <-> pos p2) ^3
                      in top </ bot


p1 = P (vector' [1..3]) (vector' [0,0,0]) 9
p2 = P (vector' [4..6]) (vector' [0,0,0]) 9
