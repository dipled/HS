module Arvore where

data Arv a = Nulo | Node a (Arv a) (Arv a)
    deriving (Show, Eq, Ord)

instance Functor Arv where
    fmap f Nulo = Nulo
    fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

instance Applicative Arv where
    pure x = Node x Nulo Nulo
    Nulo <*> _ = Nulo
    _ <*> Nulo = Nulo
    (Node fa fl fr) <*> (Node x y z) = (Node (fa x) (fl <*> y) (fr <*> z))

preorder :: Arv a -> [a]

preorder Nulo = []
preorder (Node x l r) = x : (preorder l) ++ (preorder r)

inorder :: Arv a -> [a]

inorder Nulo = []
inorder (Node x l r) = (inorder l) ++ [x] ++ (inorder r)

postorder :: Arv a -> [a]

postorder Nulo = []
postorder (Node x l r) = (postorder l) ++ (postorder r) ++ [x]

--
--calculaAltura' :: (Ord a, Ord b, Num b) => Arv (a,b) -> b -> b
--calculaAltura' Nulo count = count
--calculaAltura' (Node x l r) count = count + (max (calculaAltura' l 1) (calculaAltura' r 1))
--
    --calculaAltura :: (Ord a,Ord b, Num b) => Arv (a,b) -> b
--calculaAltura (Node (v,h) l r) = calculaAltura' (Node (v,h) l r) h
--
--
--insertElemAVL :: (Ord a, Ord b, Num b) => Arv (a,b) -> a -> Arv (a,b)
--insertElemAVL Nulo e = (Node (e,0)) Nulo Nulo
--insertElemAVL (Node (v,h) l r) e 
--    |e > v = (Node (v,h) l (insertElemAVL r e))
--    |e < v = (Node (v,h) (insertElemAVL l e) r)
--    |otherwise = (Node (v,h) l r) 


insertElem :: Ord a => Arv a -> a -> Arv a
insertElem Nulo e = (Node e) Nulo Nulo
insertElem (Node x l r) e
    |e > x = (Node x l (insertElem r e))
    |e < x = (Node x (insertElem l e) r)
    |otherwise = (Node x l r) 

insertSubarvore :: Ord a => Arv a -> Arv a -> Arv a

insertSubarvore Nulo Nulo  = Nulo
insertSubarvore Nulo (Node x1 l1 r1)  = (Node x1 l1 r1) 
insertSubarvore (Node x l r) Nulo = (Node x l r)
insertSubarvore (Node x l r) (Node x1 l1 r1) 
    |x == x1 = (Node x (insertSubarvore l l1) (insertSubarvore r r1))
    |x < x1 = (Node x l (insertSubarvore r (Node x1 l1 r1)))
    |x > x1 = (Node x (insertSubarvore l (Node x1 l1 r1)) r)

removeSubArv :: Ord a => Arv a -> a -> Arv a
removeSubArv Nulo x = Nulo
removeSubArv (Node x l r) e
    |e == x = Nulo
    |e > x = (Node x l (removeSubArv r e))
    |e < x = (Node x (removeSubArv l e) r)

removeElem' :: Ord a => Eq b => Num b => Arv a -> a -> b -> Arv a
removeElem' Nulo x _= Nulo
removeElem' (Node x l r) e before
    |e == x && before == 1 = insertSubarvore l r
    |e == x && before == -1 = insertSubarvore r l
    |e > x = (Node x l (removeElem' r e 1))
    |e < x = (Node x (removeElem' l e (-1)) r)

removeElem :: Ord a => Arv a -> a -> Arv a
removeElem arv e = removeElem' arv e 1

tree1 = Node 5 (Node 4 (Node 3 Nulo (Node 2 Nulo Nulo) ) (Nulo)) (Node 6 Nulo (Node 9 (Node 7 Nulo Nulo) (Node 10 Nulo Nulo)))
tree2 = Node 5 (Node 4 (Node 3 Nulo (Node 2 Nulo Nulo) ) (Nulo)) (Node 6 Nulo (Node 9 (Node 7 Nulo Nulo) (Node 10 Nulo Nulo)))

