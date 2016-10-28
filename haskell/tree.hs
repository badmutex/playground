import Prelude hiding (elem, lookup)

data BTree a = Leaf | Node a (BTree a) (BTree a)

insert :: Ord a => a -> BTree a -> BTree a
insert a Leaf = Node a Leaf Leaf
insert a (Node b left right) = if a < b
                               then Node b (insert a left) right
                               else Node b left (insert a right)


lookup a Leaf = Nothing
lookup a (Node b l r) = case compare a b of
                          LT -> lookup a l
                          EQ -> Just b
                          GT -> lookup a r


elem :: (Ord a) => a -> BTree a -> Bool
elem a tree = case lookup a tree of
                Nothing -> False
                Just _  -> True


fromList :: Ord a => [a] -> BTree a
fromList = foldr insert Leaf



class Truthiness a where
    truth :: a -> Bool

instance Truthiness [a] where
    truth [] = False
    truth _  = True

instance Truthiness Integer where
    truth 0 = False
    truth _ = True

instance Truthiness (BTree a) where
    truth Leaf = False
    truth _    = True


if' p a b = if truth p then a else b

