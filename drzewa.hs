import Data.Tree
data BTree a = BNil | BNode a (BTree a) (BTree a) deriving Show

ones = 1:ones

-- generator z argumentem
-- ones a = (:) a ones

-- ones = (:) 1 ones


binary = Node 1 [binary,binary]
getX :: Int -> Tree a -> Tree a
getX 0 _ = Node 0 []
getX n (Node a [l,p]) = Node a [(getX(n-1) l),(getX(n-1) p)]


bintree a = BNode a (bintree a) (bintree a)

getN :: Int -> BTree a -> BTree a
getN 0 _ = BNil
getN n (BNode a l p) = BNode a (getN(n-1) l) (getN(n-1) p) 


-- replicate



