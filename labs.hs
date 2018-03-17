-- Task 1.1

my_head (h:t) = h
my_tail (h:t) = t

-- Custom ++ operator
xs @@ ys = case (xs, ys) of
  ([], l) -> l
  (h1:[], l) -> h1:l
  (h1:t1, l) -> h1:(t1 @@ l)

my_take n xs
  | n <= 0 = []
  | xs == [] = []
  | otherwise = h:(my_take (n-1) t)
    where h:t = xs

my_drop n xs
  | n <= 0 = xs
  | xs == [] = []
  | otherwise = my_drop (n-1) t
    where h:t = xs

my_filter p xs
  | xs == [] = []
  | otherwise = if p h then h:(my_filter p t) else my_filter p t
    where h:t = xs

my_map f xs
  | xs == [] = []
  | otherwise = (f h):(my_map f t)
    where h:t = xs

-- Task 1.2
inits xs = [take i xs | i <- [0..(length xs)]]

-- Task 1.3
partitions xs = [((take i xs), (drop i xs)) | i <- [0..(length xs)]]

-- Task 1.4

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs =
  concat [map ([(xs !! i)] ++) (permutations ((take i xs) ++ (drop (i+1) xs)))
  | i <- [0..(length xs - 1)]]

-- Simplified (and a bit different) version:
permutations' [] = [[]]
permutations' (h:t) =
  let insertAt e i xs = if i <= 0 then e:xs else h:(insertAt e (i-1) t) where h:t = xs
  in concat[map(insertAt h i) (permutations' t) | i <- [0..(length t)]]

-- Task 1.5
nub [] = []
nub xs = nubAux xs []
  where nubAux [] uniqs = []
        nubAux (h:t) uniqs = if h `elem` uniqs
                             then nubAux t uniqs
                             else h:(nubAux t (h:uniqs))
-- Or simpler:

nub l = foldr (condJoin) [] l
  where condJoin h t = if not (h `elem` t) then h:t else t
