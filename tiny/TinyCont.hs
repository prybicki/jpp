{---------------------------------------------- 
 Semantyka denotacyjna języka TINY, styl kontynuacyjny
-----------------------------------------------}

import Data.Map

-- SYNTAX --

data Exp = NumExp Int
         | VarExp String
         | PlusExp Exp Exp
         | MinusExp Exp Exp
         | TimesExp Exp Exp
    
data BExp = TrueBExp
          | FalseBExp
          | LEqBExp Exp Exp
          | AndBExp BExp BExp

data Stmt = SkipStmt
          | AssStmt String Exp
          | SeqStmt Stmt Stmt
          | IfStmt BExp Stmt Stmt
          | WhileStmt BExp Stmt

-- AUXILIARY DOMAINS --

type State = Map String Int
type Ans = Int
type Cont = State -> Ans
type ECont = Int -> Ans
type BCont = Bool -> Ans

-- SEMANTIC FUNCTIONS --

semE :: Exp -> ECont -> State -> Ans
semE (NumExp n) k s = k n
semE (VarExp v) k s = k (s ! v) 
semE (PlusExp e1 e2) k s = semE e1 (\n -> semE e2 (\m -> k (n+m)) s) s
semE (MinusExp e1 e2) k s = semE e1 (\n -> semE e2 (\m -> k (n-m)) s) s
semE (TimesExp e1 e2) k s = semE e1 (\n -> semE e2 (\m -> k (n*m)) s) s

semB :: BExp -> BCont -> State -> Ans
semB TrueBExp k s = k True
semB FalseBExp k s = k False
semB (LEqBExp e1 e2) k s = semE e1 (\n -> semE e2 (\m -> k (n <= m)) s) s
semB (AndBExp b1 b2) k s = semB b1 (\v -> semB b2 (\w -> k (v && w)) s) s

semS :: Stmt -> Cont -> State -> Ans
semS SkipStmt k s = k s
semS (AssStmt v e) k s = 
       semE e (\n -> k (insert v n s)) s
semS (SeqStmt i1 i2) k s = 
       semS i1 (semS i2 k) s
semS (IfStmt b i1 i2) k s = 
       semB b (\v -> if v then semS i1 k s else semS i2 k s) s
semS (WhileStmt b i) k s =
       semB b (\v ->
                if v then semS i (\s' -> semS (WhileStmt b i) k s') s
                else k s) s

-- EXAMPLE: FIBONACCI NUMBERS --

{-- Calculate the n+1'th Fibonacci number
x := 0; y := 1; i := 0;
while i<=n do
  tmp := x+y;
  x := y;
  y := tmp;
  i := i+1;
--}

fibStmt :: Stmt
fibStmt = 
  SeqStmt
    (AssStmt "x" (NumExp 0))
    (SeqStmt 
       (AssStmt "y" (NumExp 1))
       (SeqStmt
          (AssStmt "i" (NumExp 0))
          (WhileStmt 
             (LEqBExp (VarExp "i") (VarExp "n"))
             (SeqStmt
                (AssStmt "tmp" (PlusExp (VarExp "x") (VarExp "y")))
                (SeqStmt
                   (AssStmt "x" (VarExp "y"))
                   (SeqStmt
                     (AssStmt "y" (VarExp "tmp"))
                     (AssStmt "i" (PlusExp (VarExp "i") (NumExp 1)))
                   )
                )
             )
          )
       )
    )


fib :: Int -> Int
fib n = semS fibStmt (\s -> s ! "x") (singleton "n" n)

