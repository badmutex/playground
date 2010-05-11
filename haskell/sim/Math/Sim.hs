module Math.Sim where

import Prelude as P

import Math.Vector as V

import Data.Vector (Vector)


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



forceOn :: Particle -> Particle -> Force
forceOn p1 p2 = (mass p1 * mass p2) *> (pos p1 <-> pos p2) </ (norm (pos p1 <-> pos p2) ^3)



p1 = P (vector [0,0,0]) (vector [0,0,0]) 9
p2 = P (vector [1,1,1]) (vector [0,0,0]) 9
