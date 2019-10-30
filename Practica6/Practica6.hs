module Practica6 where

-- 1
-- recibe un elemento e y una lista L. Regresa True si n esta en la lista L, False en otro caso
belongs :: Int -> [Int] -> Bool
belongs x [] = False
belongs n (x:xs) = if n == x then True else belongs n xs

-- 2
-- recibe un número n y una lista L. Regresa el n-ésimo elemento de la lista L.
get_nth :: Int -> [a] -> a
get_nth 0 (x:_) = x
get_nth n (x:xs) = get_nth(n+1)xs


-- 3
-- recibe un número n y una lista L. Regresa la lista L después de eliminar el n-ésimo elemento.
delete_nth :: Int -> [a] -> [a]
delete_nth 0 (x:xs) = xs
delete_nth n (x:xs) = [x] ++ delete_nth(n-1)xs 


data BTree a = Empty | Node a (BTree a) (BTree a) deriving (Eq, Show)

-- 4
-- recibe un árbol binario T y regresa el número de elementos de T
num_elem :: BTree a -> Int
num_elem Empty = 0
num_elem (Node e l r) = 1 + num_elem(l) + num_elem(r)

-- 5
-- recibe un elemento e y un árbol binario T. Regresa True si e es elemento de T, F alse en otro caso.
belongsT :: (Eq a) => a -> BTree a -> Bool
belongsT n Empty = False
belongsT n (Node x l r) = if n == x then True else ((belongsT n l) || (belongsT n r))

-- 6 
-- recibe un un árbol binario T y regresa la lista de sus elementos recorridos en preorden.
preorder :: BTree a -> [a]
preorder Empty = []
preorder (Node x l r) = [x]++(preorder l)++(preorder r)

-- 7
-- recibe un un árbol binario T y regresa la lista de sus elementos recorridos en preorden.
inorder :: BTree a -> [a]
inorder Empty = []
inorder (Node x l r) = (preorder l)++ [x] ++(preorder r)

-- 8
-- recibe un un árbol binario T y regresa la lista de sus elementos recorridos en preorden.
postorder :: BTree a -> [a]
postorder Empty = []
postorder (Node x l r) = (postorder l)++(postorder r)++[x]