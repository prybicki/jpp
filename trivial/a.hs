
insertAt e 0 l = e:l
insertAt e i (h:t) = h:(insertAt e (i-1) t)

-- remove (h:t)
-- moveToHead (h:t) n = ((h:t) !! n):(remove )

permutations [] = [[]]
permutations (h:[]) = [[h]]
permutations (h:t) = [insertAt h i l | l <- permutations t, i <- [0..(length l)]]

permutations' [] = [[]]
permutations' (h:t) = concat[map(insertAt h i) (permutations' t) | i <- [0..(length t)]]

-- to dziala, ale bierze zawsze ostatni element,
-- z uzyciem foldl + reverse mozna zachować kolejność
condJoin h t = if not (h `elem` t) then h:t else t
nub l = foldr (condJoin) [] l

naive_fibo 0 = 1
naive_fibo 1 = 1
naive_fibo n = naive_fibo (n-1) + naive_fibo (n-2)


fibo n = fst (fib2 n)
  where fib2 n = if n == 0 then (1,1) else let (x, y) = fib2 (n-1) in (y, x+y)

-- iv) write a function packing, which packs consecutive equal elements in a list into sublists
-- packing ["a","a","a","a","b","c","c","a","a","d","d","e","e","e","e"] = [["a","a","a","a"],["b"],["c","c"],["a","a"],["d","d"],["e","e","e","e"]]","d","d","e","e","e","e"] = [["a","a","a","a"],["b"],["c","c"],["a","a"],["d","d"],["e","e","e","e"]]

packing [] = []
packing l = sublist:(packing rest) where
  (sublist, rest) = pack (head l) l []
  pack e [] sublist = (sublist, [])
  pack e (h:t) sublist =
    if h == e then
      pack e t (e:sublist)
      else
        (sublist, h:t)


-- v) write a function positions which takes as an argument a character a, a string s, and returns the list of integers n such that the nth indexes of s is a
-- positions 'e' "Dlaczego jest tak zimno w Polsce?" = [5,10,31]

positions char list = reverse(pos char list 0 []) where
  pos c [] idx rv = rv
  pos c (h:t) idx rv = if h == c then pos c t (idx+1) (idx:rv) else pos c t (idx+1) rv

  -- vi) write a function indexOf which takes as an argument a character c, a string s, and returns a Maybe Int: Nothing if c doesn't appear in s, and Just n if n is such that the nth index of s is c

  -- indexOf 'a' "Czy ma Ana kota?" = Just 5 (or Just 9, or Just 14)
  -- indexOf 'e' "Czy ma Ana kota?" = Nothing




-- LAB #3 08.03.2018

data Tree a = Empty | Node a (Tree a) (Tree a)
instance Eq a => Eq (Tree a) where
   Empty == Empty = True
   Node a l1 r1 == Node b l2 r2 = (a == b && l1 == l2 && r1 == r2)
   _ == _ = False

instance Show a => Show (Tree a) where
  show Empty = "()"
  show (Node a l r) = "(" ++ show a ++ ", " ++ show l ++ ", " ++ show(r) ++ ")"

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

toList Empty = []
toList (Node a l r) = toList(l) ++ (a:toList(r))

insert a Empty = (Node a Empty Empty)
insert a (Node x l r) =
  if a <= x
    then (Node x (insert a (l)) r)
    else (Node x l (insert a (r)))

contains a Empty = False
contains a t = a `elem` toList t


-- Zadanie #2
