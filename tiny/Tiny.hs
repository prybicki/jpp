{---------------------------------------------- 
 Semantyka denotacyjna jezyka TINY
-----------------------------------------------}

{----------------------------------------------
 Semantyka zapisana w jezyku Haskell.
 
 Po uruchomieniu interpretera Haskella (ghci) 
 nalezy zaladowac plik Tiny.hs.
 Do testowania przykladu sluzy funkcja fib :: Int -> Int. 

 Przykladowa sesja:
 
 $ghci
 GHCi, version 7.10.3: 
 Prelude> :load Tiny.hs
 [1 of 1] Compiling Main             ( Tiny.hs, interpreted )
 Ok, modules loaded: Main.
 *Main> fib 5
 8
 *Main> fib 40
 165580141
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
          | NotBExp BExp
          | AndBExp BExp BExp

data Stmt = SkipStmt
          | AssStmt String Exp
          | SeqStmt Stmt Stmt
          | IfStmt BExp Stmt Stmt
          | WhileStmt BExp Stmt

-- AUXILIARY DOMAINS --

type State = Map String Int

-- SEMANTIC FUNCTIONS --

semE :: Exp -> State -> Int
semE (NumExp n) s = n
semE (VarExp v) s = s ! v 
semE (PlusExp e1 e2) s = (semE e1 s) + (semE e2 s)
semE (MinusExp e1 e2) s = (semE e1 s) - (semE e2 s)
semE (TimesExp e1 e2) s = (semE e1 s) * (semE e2 s)

semB :: BExp -> State -> Bool
semB TrueBExp s = True
semB FalseBExp s = False
semB (LEqBExp e1 e2) s = (semE e1 s) <= (semE e2 s)
semB (NotBExp b) s = not (semB b s)
semB (AndBExp b1 b2) s = (semB b1 s) && (semB b2 s)

semS :: Stmt -> State -> State
semS SkipStmt s = s
semS (AssStmt v e) s = 
       let n = semE e s in
       insert v n s
semS (SeqStmt i1 i2) s = 
       semS i2 (semS i1 s)
semS (IfStmt b i1 i2) s = 
       if semB b s
       then semS i1 s
       else semS i2 s
semS (WhileStmt b i) s =
       if semB b s
       then let s' = semS i s
            in semS (WhileStmt b i) s'
       else s

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
fib n = 
  (semS fibStmt (singleton "n" n)) ! "x"



