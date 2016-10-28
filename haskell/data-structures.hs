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


data Stack a = Tail | Front a (Stack a) deriving Show

push :: a -> Stack a -> Stack a
push a s = Front a s

peak :: Stack a -> Maybe a
peak Tail = Nothing
peak (Front a _) = Just a

pop :: Stack a -> Maybe (a, Stack a)
pop Tail = Nothing
pop (Front a s) = Just (a,s)

mapStack :: (a -> b) -> Stack a -> Stack b
mapStack _ Tail = Tail
mapStack f (Front a s) = Front (f a) (mapStack f s)

instance Functor Stack where fmap = mapStack

fromListStack :: [a] -> Stack a
fromListStack = foldr push Tail

toListStack :: Stack a -> [a]
toListStack Tail = []
toListStack (Front a s) = a : toListStack s

reverseStack :: Stack a -> Stack a
reverseStack s = reverseStack' s Tail
    where reverseStack' (Front a Tail) s = push a s
          reverseStack' (Front a s) s'   = reverseStack' s (push a s')
