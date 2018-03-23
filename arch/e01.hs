mul x y = x * y
square x = mul x x
area r = pi * square r

my_head0 [] = error "empty list"
my_head0 (h:_) = h
my_head1 l = case l of { [] -> error " "; h:t -> h }

my_drop n l
 | (n == 0) = l
 | otherwise = let (_:t) = l in my_drop (n-1) t


my_take 0 = 0


my_concat [] l = l
my_concat (h:t) l = h (my_concat t l)


