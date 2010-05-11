{-# LANGUAGE
  DeriveFunctor,
  EmptyDataDecls,
  FlexibleContexts,
  FlexibleInstances,
  MultiParamTypeClasses
  #-}

module Math.Vector ( Z, S, D0, D1, D2, D3
                   , previous
                   , Count (..)
                   , Tagged (..)
                   , tag
                   , Vec
                   , Vectorize (..)

                   , module V
                   )
    where

import Data.Vector as V
import Data.Foldable as F
import Data.Traversable as T

data Z
data S a

type D0 = Z
type D1 = S D0
type D2 = S D1
type D3 = S D2


previous :: S a -> a
previous = const undefined

class Count a where count :: a -> Int

instance Count Z where count = const 0
instance Count a => Count (S a) where count a = 1 + count (previous a)


newtype Tagged t a = Tag {untag :: a} deriving (Functor, Show)

tag :: Tagged t a -> t
tag = const undefined

instance Foldable Vector where foldr = V.foldr



type Vec d v a = Tagged d (v a)

class Vectorize d v a where
    (<+>) :: Num a => Vec d v a -> Vec d v a -> Vec d v a
    (<->) :: Num a => Vec d v a -> Vec d v a -> Vec d v a
    (*>)  :: Num a =>         a -> Vec d v a -> Vec d v a
    (<*)  :: Num a => Vec d v a ->         a -> Vec d v a
    (<*)   = flip (*>)
    norm  :: Floating a => Vec d v a -> a

instance Functor Vector where fmap = V.map

instance Count d => Vectorize d Vector a where
    (<+>) = vecOp (+)
    (<->) = vecOp (-)
    (*>)  = scalarMul
    norm  = sqrt . V.sum . V.map (^2) . untag



vecOp :: (a -> b -> c) -> Tagged t (Vector a) -> Tagged t (Vector b) -> Tagged t (Vector c)
vecOp op a b = let (x,y) = (untag a, untag b)
               in Tag $ V.zipWith op x y


scalarMul :: (Count t, Num n) => n -> Tagged t (Vector n) -> Tagged t (Vector n)
scalarMul a v = vecOp (*) (Tag $ V.replicate dim a) v
    where dim = fromIntegral . count $ tag v
