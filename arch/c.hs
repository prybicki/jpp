data Exp
  = EInt Int             -- stała całkowita
  | EAdd Exp Exp         -- e1 + e2
  | ESub Exp Exp         -- e1 - e2
  | EMul Exp Exp         -- e1 * e2
  | EVar String          -- zmienna
  | ELet String Exp Exp  -- let var = e1 in e2

instance Eq Exp where
  (EInt n) == (EInt m) = n == m
  (EAdd e1 f1) == (EAdd e2 f2) = e1 == e2 && f1 == f2
  (ESub e1 f1) == (ESub e2 f2) = e1 == e2 && f1 == f2
  (EVar x) == (EVar y) = x == y
  (ELet x1 e1 f1) == (ELet x2 e2 f2) = x1 == x2 && e1 == e2 && f1 ==f2


instance Show Exp where
  show (EInt i) = show i
  show (EAdd i j) = "(" ++ (show i) ++ ") + (" ++ (show j) ++ ")"
  -- ^^ same for EMul, ESub
  show (EVar s) = s
  show (ELet x i j) = "var" ++ x ++ " = " ++ (show i) ++ " in " ++ (show j)

instance Num Exp where
  fromInteger n = EInt(fromInteger n)
  a + b = EAdd a b
  a - b = ESub a b
  a * b = EMul a b
  negate a = ESub(EInt 0) a


simpl (EInt i) = Eint i
simpl (EVar x) = EVar x
simpl (ELet x e1 e2) = ELet x (simpl e1) (simpl e2)
simpl (EAdd e f) =
  let (e', f') = (simpl e, simpl f) in
  case (e', f') of {
    (EInt i, EInt j) -> EInt(i+j);
    (EInt i, _) -> f';
    (_, _) -> EAdd(e', f')
  }

  -- ...
  -- (EInt i, EInt j) -> EInt(i * j);
  -- (EInt 0, _) -> EInt 0
  -- (_, EInt 0) -> EInt 0
  -- (EInt 1, _) -> f'
  -- (_, EInt 1) -> e'
  -- (_, _) -> EMul e' f'

  -- ESub analogicznie

deriv _ (EInt _) = 0
deriv x (EVar y)
  | x == y = y
  | otherwise 0
deriv x (EAdd e f) = EAdd (deriv x e) (deriv x f)
deriv x (EMul e f) = EAdd (EMul e (deriv x f)) (EMul (deriv x e) f)
deriv x (ELet y e f) = ELet y e (deriv x f)
