module Tree where

data BinTree a = Nil | Node a (BinTree a) (BinTree a) 
    deriving (Show, Eq, Ord, Read)
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

-- **************************************************Basic**************************************************


preorder :: BinTree a -> [a]
preorder Nil = []
preorder (Node x l r) = x : preorder l ++ preorder r

inorder :: BinTree a -> [a]
inorder Nil = []
inorder (Node x l r) = inorder l ++ [x] ++ inorder r

postorder :: BinTree a -> [a]
postorder Nil = []
postorder (Node x l r) = postorder l ++ postorder r ++ [x]

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
    |e == x && before == -1 = Nil
    |e > x = Node x l (removeElem' r e 1)
    |e < x = Node x (removeElem' l e (-1)) r

removeElem :: Ord a => BinTree a -> a -> BinTree a
removeElem arv e = removeElem' arv e 1

-- **************************************************AVL**************************************************

-- atualizaAltura :: Ord a => BinTree (a,Int) -> a -> BinTree (a,Int)
-- atualizaAltura (Node (v,h) l r) e
--     |e > v = Node (v,h+1) l (atualizaAltura r e) 
--     |e < v = Node (v,h+1) (atualizaAltura l e) r
--     |otherwise = Node (v,h) l r


-- insertElemAVL :: Ord a => BinTree (a, Int) -> a -> BinTree (a, Int)
-- insertElemAVL Nil e = Node (e,0) Nil Nil
-- insertElemAVL (Node (v,h) l r) e
--     |e > v = Node (v,h) l (insertElemAVL r e)
--     |e < v = Node (v,h) (insertElemAVL l e) r
--     |otherwise = Node (v,h) l r 



tree1 = Node 5 (Node 4 (Node 3 (Node 2 Nil Nil) Nil) Nil) (Node 6 Nil (Node 9 (Node 7 Nil Nil) (Node 10 Nil Nil)))
tree2 = (+) 1 <$> tree1 --Sums one to every single element of tree
tree3 = (+) <$> tree1 <*> tree2 -- very cool shit we're able to do cuz of Applicative
menu = "Digite 1 para adicionar um elemento\n2 para remover um elemento\n3 printar a arvore"

insere t = 
    do
        putStrLn "Digite o numero para adicionar"
        r <- getLine
        let result = read r
        let newT = insertElem t result
        partida newT

remove t = 
    do
        putStrLn "Digite o numero para remover"
        r <- getLine
        let result = read r
        let newT = removeElem t result
        partida newT

partida :: (Show a, Ord a, Read a) => BinTree a -> IO ()
partida t = 
    do

        putStrLn menu
        r <- getLine
        case (read r) of
           1 -> insere t
           2 -> remove t
           3 -> do print t; partida t

main :: IO ()
main =
    do
        putStrLn "Qual o tipo de arvore voce quer criar? (Int, Float, String)"
        r <- getLine
        case r of
            "Float" -> partida (Nil :: BinTree Float)
            "Int" -> partida (Nil :: BinTree Int)
            "String" -> partida (Nil :: BinTree String)
            
