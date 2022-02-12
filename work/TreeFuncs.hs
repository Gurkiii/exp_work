module TreeFuncs where

{-a general Tree structure for content of type a in each node and leaf-}
data ContentTree a = Leaf a | Node (ContentTree a) a (ContentTree a) deriving Show

{-an infinite tree which contains tuples of integers stating how many times
the tree has been walked right or left to get to that specific node or leaf-}
numTree :: ContentTree (Integer, Integer)
numTree = Node (subNumTree (1, 0)) (0, 0) (subNumTree (0, 1))

subNumTree :: (Integer, Integer) -> ContentTree (Integer, Integer)
subNumTree (l, r) = Node (subNumTree (l+1, r)) (l, r) (subNumTree (l, r+1))

{-a function similar to the take function for lists to cut the tree at a specific height n-}
cutTree :: Integer -> ContentTree a -> ContentTree a
cutTree n tree = if n > 0
    then subcutTree n tree
    else error "illegal Argument, height to cut tree at must be positive"

subcutTree :: Integer -> ContentTree a -> ContentTree a
subcutTree _ (Leaf cont) = Leaf cont
subcutTree n (Node a cont b) = if n == 1
    then Leaf cont
    else Node (subcutTree (n-1) a) cont (subcutTree (n-1) b)

