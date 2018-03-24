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
  -- l1 :| l2 === r1 :| r2 = let (l1' :| l2') = simpl (l1 :| l2)
  --                             (r1' :| r2') = simpl (r1 :| r2)
  --                         in  (l1' === r1' && l2' === r2')
  --                         ||  (l1' === r2' && l2' === r1')
  -- l1 :| l2 === r = simpl (l1 :| l2) == simpl r
  -- l === r1 :| r2 = r1 :| r2 === l
  -- l1 :> l2 === r1 :> r2 = let (l1' :| l2') = simpl (l1 :| l2)
  --                             (r1' :| r2') = simpl (r1 :| r2)
  --                         in  l1' === r1' && l2' === r2'

instance Mon (Reg c) where
  m1 = Eps
  x <> y = x :> y

simpl :: Reg c -> Reg c
-- simpl (Empty :> _) = Empty
-- simpl (Empty :| x) = x
-- simpl (Eps :> x) = x
-- simpl (Many x) = Many (simpl x)
simpl (l :> r) = foldl (concat') Eps (linerizeConcat (l :> r)) where
    concat' Eps x = x
    concat' acc x = acc :> x
    linerizeConcat (l :> r) = (linerizeConcat (simpl l)) ++ (linerizeConcat (simpl r))
    linerizeConcat Eps = []
    linerizeConcat x = [x]
simpl (l :| r) = foldl (sum') Empty (linearizeSum (l :| r)) where
  sum' Empty x = x
  sum' acc x = acc :| x
  linearizeSum (l :| r) = (linearizeSum (simpl l)) ++ (linearizeSum (simpl r))
  linearizeSum Empty = []
  linearizeSum x = [x]
simpl x = x

-- simpl (l :| Empty) = Empty
-- simpl (Empty :| r) = Empty
-- simpl (x :| Eps) = if nullable x then simpl x else (simpl x :| simpl Eps)
-- simpl (Eps :| x) = simpl (x :| Eps)
-- simpl (l :| r) = (simpl l :| simpl r)
--
-- simpl (l :> Empty) = simpl l
-- simpl (Empty :> r) = simpl r
-- simpl (l :> Eps) = simpl l
-- simpl (Eps :> r) = simpl r
-- simpl (l :> r) = (simpl l :> simpl r)

simpl (Many r) = Many (simpl r)
simpl r = r   -- Lit, Eps, Empty

nullable :: Reg c -> Bool
nullable Eps = True
nullable (Many _) = True
nullable (l :| r) = (nullable l) || (nullable r)
nullable (l :> r) = (nullable l) && (nullable r)
nullable _ = False

empty :: Reg c -> Bool
empty Empty = True
empty (l :| r) = (empty l) && (empty r)
empty (l :> r) = (empty l) && (empty r)
empty _ = False

der :: c -> Reg c -> Reg c
der c r = r

ders :: Eq c => [c] -> Reg c -> Reg c
ders c r = r

accepts :: Eq c => Reg c -> [c] -> Bool
accepts r w = False

mayStart :: Eq c => c -> Reg c -> Bool
mayStart c r = False

match :: Eq c => Reg c -> [c] -> Maybe [c]
match r w = Nothing

search :: Eq c => Reg c -> [c] -> Maybe [c]
search r w = Nothing

findall :: Eq c => Reg c -> [c] -> [[c]]
findall r w = []

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
