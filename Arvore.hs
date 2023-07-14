module Tree where

data BinTree a = Nil | Node a (BinTree a) (BinTree a) 
    deriving (Show, Eq, Ord)
instance Functor BinTree where
    fmap :: (a -> b) -> BinTree a -> BinTree b
    fmap f Nil = Nil
    fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

instance Applicative BinTree where
    pure :: a -> BinTree a
    pure x = Node x Nil Nil

    (<*>) :: BinTree (a -> b) -> BinTree a -> BinTree b
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Node fa fl fr) <*> (Node x y z) = Node (fa x) (fl <*> y) (fr <*> z)

--instance Monad BinTree where
--    
--    Nil >>= f = Nil
--    (Node a l r) >>= f = insertSubtree (insertSubtree (f a) (l >>= f)) (r >>= f)
--
sumTree :: (Num a, Ord a) => BinTree a -> BinTree a -> BinTree a
sumTree a1 a2 = (fmap (+) a1) <*> a2

preorder :: BinTree a -> [a]
preorder Nil = []
preorder (Node x l r) = x : preorder l ++ preorder r

inorder :: BinTree a -> [a]
inorder Nil = []
inorder (Node x l r) = inorder l ++ [x] ++ inorder r

postorder :: BinTree a -> [a]
postorder Nil = []
postorder (Node x l r) = postorder l ++ postorder r ++ [x]

--
--calculaAltura' :: (Ord a, Ord b, Num b) => BinTree (a,b) -> b -> b
--calculaAltura' Nil count = count
--calculaAltura' (Node x l r) count = count + (max (calculaAltura' l 1) (calculaAltura' r 1))
--
--calculaAltura :: (Ord a,Ord b, Num b) => BinTree (a,b) -> b
--calculaAltura (Node (v,h) l r) = calculaAltura' (Node (v,h) l r) h
--
--
--insertElemAVL :: (Ord a, Ord b, Num b) => BinTree (a,b) -> a -> BinTree (a,b)
--insertElemAVL Nil e = (Node (e,0)) Nil Nil
--insertElemAVL (Node (v,h) l r) e 
--    |e > v = (Node (v,h) l (insertElemAVL r e))
--    |e < v = (Node (v,h) (insertElemAVL l e) r)
--    |otherwise = (Node (v,h) l r) 


insertElem :: Ord a => BinTree a -> a -> BinTree a
insertElem Nil e = Node e Nil Nil
insertElem (Node x l r) e
    |e > x = Node x l (insertElem r e)
    |e < x = Node x (insertElem l e) r
    |otherwise = Node x l r 

insertSubtree :: Ord a => BinTree a -> BinTree a -> BinTree a
insertSubtree Nil Nil  = Nil
insertSubtree Nil (Node x1 l1 r1)  = Node x1 l1 r1 
insertSubtree (Node x l r) Nil = Node x l r
insertSubtree (Node x l r) (Node x1 l1 r1) 
    |x == x1 = Node x (insertSubtree l l1) (insertSubtree r r1)
    |x < x1 = Node x l (insertSubtree r (Node x1 l1 r1))
    |x > x1 = Node x (insertSubtree l (Node x1 l1 r1)) r

removeSubBinTree :: Ord a => BinTree a -> a -> BinTree a
removeSubBinTree Nil x = Nil
removeSubBinTree (Node x l r) e
    |e == x = Nil
    |e > x = Node x l (removeSubBinTree r e)
    |e < x = Node x (removeSubBinTree l e) r

removeElem' :: Ord a => Eq b => Num b => BinTree a -> a -> b -> BinTree a
removeElem' Nil x _= Nil
removeElem' (Node x l r) e before
    |e == x && before == 1 = insertSubtree l r
    |e == x && before == -1 = insertSubtree r l
    |e > x = Node x l (removeElem' r e 1)
    |e < x = Node x (removeElem' l e (-1)) r

removeElem :: Ord a => BinTree a -> a -> BinTree a
removeElem arv e = removeElem' arv e 1

tree1 = Node 5 (Node 4 (Node 3 Nil (Node 2 Nil Nil) ) Nil) (Node 6 Nil (Node 9 (Node 7 Nil Nil) (Node 10 Nil Nil)))
tree2 = (+) 1 <$> tree1 --Sums one to every single element of tree
tree3 = (+) <$> tree1 <*> tree2 -- very cool shit we're able to do cuz of Applicative
