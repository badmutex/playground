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
                   )
    where

import qualified Data.Vector as V
import Data.Foldable as F
import Data.Traversable as T
import Prelude as P hiding (length)
import Text.Printf

data Z
data S a

type D0 = Z
type D1 = S D0
type D2 = S D1
type D3 = S D2


previous :: S a -> a
previous = const undefined

class Count a where count :: a -> Integer

instance Count Z where count = const 0
instance Count a => Count (S a) where count a = 1 + count (previous a)


newtype Tagged t a = Tag {untag :: a} deriving (Functor, Show)

tag :: Tagged t a -> t
tag = const undefined



typeDim :: (Count d, Num i) => Vec d v a -> i
typeDim = fromIntegral . count . tag . unvec
{-# INLINE typeDim #-}


instance Foldable V.Vector where foldr = V.foldr



newtype Vec d v a = MkVec { unvec :: Tagged d (v a) }
    deriving Show

unwrap :: Vec d v a -> v a
unwrap = untag . unvec

class FromList v where
    fromList :: [a] -> v a

instance FromList [] where
    fromList = id

instance FromList V.Vector where
    fromList = V.fromList




class Functor v => BaseVector v where
    length :: v a -> Int



instance BaseVector V.Vector where
    length = V.length



class Vectorize d v a where
    (<+>) :: Num a => Vec d v a -> Vec d v a -> Vec d v a
    {-# INLINE (<+>) #-}
    (<->) :: Num a => Vec d v a -> Vec d v a -> Vec d v a
    {-# INLINE (<->) #-}
    (*>)  :: Num a =>         a -> Vec d v a -> Vec d v a
    {-# INLINE (*>) #-}
    (<*)  :: Num a => Vec d v a ->         a -> Vec d v a
    {-# INLINE (<*) #-}
    (<*)   = flip (*>)
    norm  :: Floating a => Vec d v a -> a
    {-# INLINE norm #-}


instance Functor V.Vector where fmap = V.map

instance Count d => Vectorize d V.Vector a where
    (<+>) = vecOp V.zipWith (+)
    (<->) = vecOp V.zipWith (-)
    (*>)  = scalarMul V.zipWith V.replicate
    norm  = sqrt . V.sum . V.map (^2) . unwrap



vecOp :: (Count d, Functor f) =>
         ((a -> b -> c) -> f a -> f b -> f c)
      -> (a -> b -> c)
      -> Vec d f a
      -> Vec d f b
      -> Vec d f c
vecOp zipper op a b = MkVec . Tag $ zipper op (unwrap a) (unwrap b)
{-# INLINE vecOp #-}


scalarMul :: (Count d, Num a, Num i, Functor f) =>
             ((a -> a -> a) -> f a -> f a -> f a)
          -> (i -> a -> f a)
          -> a
          -> Vec d f a
          -> Vec d f a
scalarMul zipper rep a v = vecOp zipper (*) (MkVec . Tag $ rep d a) v
    where d = typeDim v
{-# INLINE scalarMul #-}




instance Count d => Vectorize d [] a where
    (<+>) = vecOp P.zipWith (+)
    (<->) = vecOp P.zipWith (-)
    (*>)  = scalarMul P.zipWith P.replicate
    norm  = sqrt . P.sum . P.map (^2) . unwrap







vector :: (Count d, FromList v, BaseVector v) =>
          [a] -> Either (Int, Int) (Vec d v a)
vector xs = let vec = MkVec . Tag $ fromList xs
                check :: (BaseVector v, Count d) => Vec d v a -> Either (Int, Int) (Vec d v a)
                check v = let l = length (unwrap v)
                              d = typeDim v
                          in if length (unwrap v) == typeDim v
                             then Right v
                             else Left (l,d)
            in check vec
{-# INLINE vector #-}

vector' :: (Count d, FromList v, BaseVector v) => [a] -> Vec d v a
vector' xs = case vector xs of
               Right v -> v
               Left (l,d) -> error $ printf "Type dimensional mismatch: length = %d, typed as %d" l d
{-# INLINE vector' #-}





v :: Vec D3 V.Vector Int
v = vector'  [1..3]
