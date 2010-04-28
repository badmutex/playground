-- | Inspired by Brent Yorgey's post here 
--  http://byorgey.wordpress.com/2010/03/03/deriving-pleasure-from-ghc-6-12-1/


{-# LANGUAGE
  DeriveFunctor,
  DeriveFoldable,
  DeriveTraversable,
  NoMonomorphismRestriction
  #-}

import Data.Maybe
import Data.Foldable
import Data.Traversable
import Prelude hiding (foldr)

data Btree a = Leaf | Node a (Btree a) (Btree a)
             deriving (Show, Eq, Functor, Foldable, Traversable)

insert :: Ord a => a -> Btree a -> Btree a
insert v Leaf = Node v Leaf Leaf
insert v (Node a l r) = if v < a
                        then Node a (insert v l) r
                        else Node a l (insert v r)

fromList = foldr insert Leaf

vs = [10,5,20,15,1,8,2,7,5]
t = fromList vs
t2 = fromList . map (*2) $ vs