module RegExtra where
import Mon
import Reg
import Data.List

data AB = A | B deriving(Eq,Ord,Show)

infix 4 ===
class Equiv a where
  (===) :: a -> a -> Bool

instance (Eq c) => Equiv (Reg c) where
  l === r = (simpl l) == (simpl r)

instance Mon (Reg c) where
  m1 = Eps
  x <> y = x :> y

-- my simpl normalizes tree by linearizing it and then foldr-ing it
-- with concatenation that catches some nilpotent elements and ignores them
-- in case of sum before folding the list is nub-bed to remove things ~ (a :| a)
simpl :: (Eq c) => Reg c -> Reg c
simpl (l :> r) = foldr (concat') Eps $ linearizeConcat (l :> r) [] where
  concat' Eps x = x
  concat' acc Eps = acc
  concat' acc Empty = Empty
  concat' Empty x = Empty
  concat' acc x = acc :> x
  linearizeConcat (l :> r) acc = linearizeConcat l (linearizeConcat r acc)
  linearizeConcat x acc = (simpl x):acc
simpl (l :| r) = foldl (sum') Empty $ nub $ linearizeSum (l :| r) [] where
  sum' acc Empty = acc
  sum' Empty x = x
  sum' acc x = acc :| x
  linearizeSum (l :| r) acc = linearizeSum l (linearizeSum r acc)
  linearizeSum x acc = (simpl x):acc
simpl (Many Eps) = Eps
simpl (Many Empty) = Eps
simpl (Many r) = Many (simpl r)
simpl r = r -- Lit, Eps, Empty

nullable :: Reg c -> Bool
nullable Eps = True
nullable (Many _) = True
nullable (l :| r) = (nullable l) || (nullable r)
nullable (l :> r) = (nullable l) && (nullable r)
nullable _ = False -- Lit, Empty

empty :: Reg c -> Bool
empty Empty = True
empty (l :| r) = (empty l) && (empty r)
empty (l :> r) = (empty l) || (empty r)
empty _ = False --  Eps, Lit, Many

der :: (Eq c) => c -> Reg c -> Reg c
der c r = simpl $ derAux c (simpl r) where
  derAux c Empty = Empty
  derAux c Eps = Empty
  derAux c (Lit a) = if c == a then Eps else Empty
  derAux c (Many r) = (derAux c r) :> (Many r)
  derAux c (l :| r) = (derAux c l) :| (derAux c r)
  derAux c (l :> r) = if nullable l
                   then ((derAux c l) :> r) :| (derAux c r)
                   else (derAux c l) :> r

ders :: Eq c => [c] -> Reg c -> Reg c
ders str r = foldl (flip der) (simpl r) str

accepts :: Eq c=> Reg c -> [c] -> Bool
accepts r w = nullable $ ders w r

mayStart :: Eq c => c -> Reg c -> Bool
mayStart c r = not $ empty $ ders [c] r

match :: Eq c => Reg c -> [c] -> Maybe [c]
match r [] = if nullable r then Just [] else Nothing
match r (c:s)
  | empty r = Nothing
  | otherwise = fromNextOutput $ match (der c r) s where
      fromNextOutput (Just s2) = Just (c:s2)
      fromNextOutput Nothing = if nullable r then Just [] else Nothing

search :: Eq c => Reg c -> [c] -> Maybe [c]
search r [] = if nullable r then Just [] else Nothing
search r (c:s) = fromNextOutput $ match r (c:s) where
  fromNextOutput (Just cs) = Just cs
  fromNextOutput Nothing = search r s

findall :: Eq c => Reg c -> [c] -> [[c]]
findall r w = filter (not . null) $ foldr (@@) [] matches where
  (Just h) @@ t = h:t
  Nothing @@ t = t
  matches = map (match r) (tails w)

char :: Char -> Reg Char
char = Lit

string :: [Char] -> Reg Char
string = foldr1 (:>) . map Lit

alts :: [Char] -> Reg Char
alts = foldr1 (:|) . map Lit

letter = alts ['a'..'z'] :| alts ['A'..'Z']
digit = alts ['0'..'9']
number = digit :> Many digit
ident = letter :> Many (letter :| digit)

many1 r = r :> Many r
