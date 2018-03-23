-- write two functions
-- inits [1, 2] = [[]. [1], [1,2]]
-- sublist [1, 2] = [[], [1], [2], [1, 2]]

sublists_helper acc [] = acc
sublists_helper acc (h:t) =
	sublists_helper ([h] : acc ++ (map (\x -> h:x) acc)) t
sublists l = sublists_helper [] l 

sublists2 [] = [[]]

sublists2 (h:t) =
	let st = sublists2 t in 
		st ++ [h:s | s <- st]

-- write a partition function 
-- partitions [1, 2, 3] = [ ([], [1,2,3]), ([1], [2,3]), ([1,2], [3]), ([1,2,3], []) ]

partition l = [(take i l, drop i l) | i <- [0..length l]]

-- write a function 2n_max which returns the second maximal element of a list of integer
-- 2nd_max [7, 7, 11, 9, 15] = 11
-- 2nd_max [1] = error!
-- Try to read the list only once

aux [] = error "..."
aux [_] = error "..."
aux [i, j] = (max i j, min i j)
aix f:t = 
	let (i, j) = aux t in
		if f > i then (f, i)
			else if f < j 
