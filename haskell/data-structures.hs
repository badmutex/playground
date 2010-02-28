import System.Random

data BinTree a = Leaf | Node a (BinTree a) (BinTree a) deriving Show

inorder,preorder,postorder :: Show a => BinTree a -> [a]

inorder   Leaf         = []
inorder   (Node a l r) = inorder l ++ [a] ++ inorder r

preorder  Leaf         = []
preorder  (Node a l r) = [a] ++ preorder l ++ preorder r

postorder Leaf         = []
postorder (Node a l r) = postorder l ++ postorder r ++ [a]


insert :: Ord a => a -> BinTree a -> BinTree a
insert a Leaf = Node a Leaf Leaf
insert a (Node b l r) = if a < b
                        then Node b (insert a l) r
                        else Node b l (insert a r)

fromListBT :: Ord a => [a] -> BinTree a
fromListBT = foldr insert Leaf

randomBT r l = do
  g <- getStdGen
  let t = fromListBT . take l $ randomRs r g
  return t


