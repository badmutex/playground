{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

import           Data.Singletons
import           GHC.TypeLits

newtype V (x :: Nat) a =
  V [a]
  deriving (Eq, Show)


v0 :: V 0 a
v0 = V []

v1 :: a -> V 1 a
v1 a = V [a]

