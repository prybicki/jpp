data Tree a = Null | Node a (Tree a) (Tree a) deriving(Eq, Show)


insert :: (Ord a) => Tree a -> a -> Tree a
insert Null e = Node e Null Null
insert (Node x l r) e = if e <= x then Node x (insert l e) r else Node x l (insert r e)

a = Node 2 (Node 1 Null Null) (Node 5 Null Null)

linearize :: (Num a) => Tree a -> [a]
linearize t = linearizeAux t [] where
  linearizeAux :: (Num a) => Tree a -> [a] -> [a]
  -- acc to lista wszystkich wierzcholkow na prawo
  linearizeAux Null acc = -1:acc
  linearizeAux (Node x Null Null) acc = x:acc
  linearizeAux (Node x l r) acc = linearizeAux l (x:(linearizeAux r acc))
