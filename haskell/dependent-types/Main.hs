{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}


import           Data.Proxy
import           Data.Reflection
import           Data.Singletons
import           Data.Singletons.Prelude
import           Data.Singletons.TypeLits
import           GHC.TypeLits

-- a type-indexed list
newtype List (x :: Nat) a = List [a] deriving (Eq, Show)


v0 :: List 0 a
v0 = List []

v1 :: a -> List 1 a
v1 a = List [a]

zeros :: (Num a, KnownNat n) => List n a
zeros = withSing go
  where
    go :: (Num a, KnownNat n) => Sing n -> List n a
    go s = let size = fromIntegral $ fromSing s
           in List $ take size $ repeat 0
