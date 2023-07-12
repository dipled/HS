data Arvore a = Nulo | Nodo a (Arvore a) (Arvore a)
    deriving (Show, Eq, Ord)


preorder :: Arvore a -> [a]

preorder Nulo = []
preorder (Nodo x l r) = x : (preorder l) ++ (preorder r)

inorder :: Arvore a -> [a]

inorder Nulo = []
inorder (Nodo x l r) = (inorder l) ++ [x] ++ (inorder r)

postorder :: Arvore a -> [a]

postorder Nulo = []
postorder (Nodo x l r) = (postorder l) ++ (postorder r) ++ [x]

insertElem :: Ord a => Arvore a -> a -> Arvore a
insertElem Nulo e = (Nodo e) Nulo Nulo
insertElem (Nodo x l r) e
    |e > x = (Nodo x l (insertElem r e))
    |e < x = (Nodo x (insertElem l e) r)
    |otherwise = (Nodo x l r) 

insertSubarvore :: Ord a => Arvore a -> Arvore a -> Arvore a

insertSubarvore Nulo Nulo  = Nulo
insertSubarvore Nulo (Nodo x1 l1 r1)  = (Nodo x1 l1 r1) 
insertSubarvore (Nodo x l r) Nulo = (Nodo x l r)
insertSubarvore (Nodo x l r) (Nodo x1 l1 r1) 
    |x == x1 = (Nodo x (insertSubarvore l l1) (insertSubarvore r r1))
    |x < x1 = (Nodo x l (insertSubarvore r (Nodo x1 l1 r1)))
    |x > x1 = (Nodo x (insertSubarvore l (Nodo x1 l1 r1)) r)

removeSubArvore :: Ord a => Arvore a -> a -> Arvore a
removeSubArvore Nulo x = Nulo
removeSubArvore (Nodo x l r) e
    |e == x = Nulo
    |e > x = (Nodo x l (removeSubArvore r e))
    |e < x = (Nodo x (removeSubArvore l e) r)

removeElem' :: Ord a => Eq b => Num b => Arvore a -> a -> b -> Arvore a
removeElem' Nulo x _= Nulo
removeElem' (Nodo x l r) e before
    |e == x && before == 1 = insertSubarvore l r
    |e == x && before == -1 = insertSubarvore r l
    |e > x = (Nodo x l (removeElem' r e 1))
    |e < x = (Nodo x (removeElem' l e (-1)) r)

removeElem :: Ord a => Arvore a -> a -> Arvore a
removeElem arv e = removeElem' arv e 1

testTree = Nodo 5 (Nodo 4 (Nodo 3 Nulo (Nodo 2 Nulo Nulo) ) (Nulo)) (Nodo 6 Nulo (Nodo 9 (Nodo 7 Nulo Nulo) (Nodo 10 Nulo Nulo)))
smaller = Nodo 99 Nulo Nulo

