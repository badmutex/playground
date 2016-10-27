{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}


import           Data.Proxy
import           Data.Reflection
import           Data.Singletons
import           Data.Singletons.Prelude
import           Data.Singletons.TypeLits
import           GHC.TypeLits

-- a type-indexed list
newtype L (x :: Nat) a = L [a] deriving (Eq, Show)

data X a where
  X :: L n a -> X a

deriving instance Show a => Show (X a)


v0 :: L 0 a
v0 = L []

v1 :: a -> L 1 a
v1 a = L [a]

-- z :: Integer
-- z = withSomeSing (42 :: Integer) go
--   where
--     go (s :: Sing a) = (fromSing s :: Integer)

-- z :: Integer -> Integer
-- z size = case toSing size of
--   SomeSing s -> fromSing s

zeros' :: (Num a, KnownNat n) => Integer -> L n a
zeros' size = L $ take (fromIntegral size) $ repeat 0

zeros :: (Num a, KnownNat n) => L n a
zeros = withSing go
  where
    go :: (Num a, KnownNat n) => Sing n -> L n a
    go s = zeros' (fromIntegral $ fromSing s)


-- x :: KnownNat n => Integer -> X n
-- x n = case toSing n of
--   SomeSing (ss :: SNat n) -> X $ take (fromIntegral n) $ repeat 0



-- x :: KnownNat n => X n
-- x n = withSomeSing n go
--   where
--     go :: KnownNat n => Sing n -> X n
--     go s = X $ take (fromIntegral $ fromSing s) $ repeat 0


  -- let s = fromIntegral $ fromSing ssize
  -- in L $ take s $ repeat 0




-- zeros :: (Num a, KnownNat n) => L n a
-- zeros = withSing go
--   where
--     go :: (Num a, KnownNat n) => Sing n -> L n a
--     go s = let size = fromIntegral $ fromSing s
--            in L $ take size $ repeat 0



-- -- a safe "head"
-- head' :: L (n :>= 1) a -> a
-- head' = undefined
