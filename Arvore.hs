module Arvore where

data Arv a = Nulo | Nodo a (Arv a) (Arv a)
    deriving (Show, Eq, Ord)

instance Functor Arv where
    fmap f Nulo = Nulo
    fmap f (Nodo a l r) = Nodo (f a) (fmap f l) (fmap f r)

instance Applicative Arv where
    pure x = Nodo x Nulo Nulo
    Nulo <*> _ = Nulo
    _ <*> Nulo = Nulo

    (Nodo fa fl fr) <*> (Nodo x y z) = (Nodo (fa x) (fl <*> y) (fr <*> z))

preorder :: Arv a -> [a]

preorder Nulo = []
preorder (Nodo x l r) = x : (preorder l) ++ (preorder r)

inorder :: Arv a -> [a]

inorder Nulo = []
inorder (Nodo x l r) = (inorder l) ++ [x] ++ (inorder r)

postorder :: Arv a -> [a]

postorder Nulo = []
postorder (Nodo x l r) = (postorder l) ++ (postorder r) ++ [x]

--
--calculaAltura' :: (Ord a, Ord b, Num b) => Arv (a,b) -> b -> b
--calculaAltura' Nulo count = count
--calculaAltura' (Nodo x l r) count = count + (max (calculaAltura' l 1) (calculaAltura' r 1))
--
    --calculaAltura :: (Ord a,Ord b, Num b) => Arv (a,b) -> b
--calculaAltura (Nodo (v,h) l r) = calculaAltura' (Nodo (v,h) l r) h
--
--
--insertElemAVL :: (Ord a, Ord b, Num b) => Arv (a,b) -> a -> Arv (a,b)
--insertElemAVL Nulo e = (Nodo (e,0)) Nulo Nulo
--insertElemAVL (Nodo (v,h) l r) e 
--    |e > v = (Nodo (v,h) l (insertElemAVL r e))
--    |e < v = (Nodo (v,h) (insertElemAVL l e) r)
--    |otherwise = (Nodo (v,h) l r) 


insertElem :: Ord a => Arv a -> a -> Arv a
insertElem Nulo e = (Nodo e) Nulo Nulo
insertElem (Nodo x l r) e
    |e > x = (Nodo x l (insertElem r e))
    |e < x = (Nodo x (insertElem l e) r)
    |otherwise = (Nodo x l r) 

insertSubarvore :: Ord a => Arv a -> Arv a -> Arv a

insertSubarvore Nulo Nulo  = Nulo
insertSubarvore Nulo (Nodo x1 l1 r1)  = (Nodo x1 l1 r1) 
insertSubarvore (Nodo x l r) Nulo = (Nodo x l r)
insertSubarvore (Nodo x l r) (Nodo x1 l1 r1) 
    |x == x1 = (Nodo x (insertSubarvore l l1) (insertSubarvore r r1))
    |x < x1 = (Nodo x l (insertSubarvore r (Nodo x1 l1 r1)))
    |x > x1 = (Nodo x (insertSubarvore l (Nodo x1 l1 r1)) r)

removeSubArv :: Ord a => Arv a -> a -> Arv a
removeSubArv Nulo x = Nulo
removeSubArv (Nodo x l r) e
    |e == x = Nulo
    |e > x = (Nodo x l (removeSubArv r e))
    |e < x = (Nodo x (removeSubArv l e) r)

removeElem' :: Ord a => Eq b => Num b => Arv a -> a -> b -> Arv a
removeElem' Nulo x _= Nulo
removeElem' (Nodo x l r) e before
    |e == x && before == 1 = insertSubarvore l r
    |e == x && before == -1 = insertSubarvore r l
    |e > x = (Nodo x l (removeElem' r e 1))
    |e < x = (Nodo x (removeElem' l e (-1)) r)

removeElem :: Ord a => Arv a -> a -> Arv a
removeElem arv e = removeElem' arv e 1

tree1 = Nodo 5 (Nodo 4 (Nodo 3 Nulo (Nodo 2 Nulo Nulo) ) (Nulo)) (Nodo 6 Nulo (Nodo 9 (Nodo 7 Nulo Nulo) (Nodo 10 Nulo Nulo)))
tree2 = Nodo 5 (Nodo 4 (Nodo 3 Nulo (Nodo 2 Nulo Nulo) ) (Nulo)) (Nodo 6 Nulo (Nodo 9 (Nodo 7 Nulo Nulo) (Nodo 10 Nulo Nulo)))

