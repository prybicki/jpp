{--------------------------------------------------
 Semantyka denotacyjna jezyka TINY z deklaracjami zmiennych
---------------------------------------------------}

{----------------------------------------------
 Semantyka zapisana w jezyku Haskell.
 
 Po uruchomieniu interpretera Haskella (ghci) 
 nalezy zaladowac plik Declarations.hs.
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

data VDecl = NoVDecl
           | SingleVDecl String VDecl

data Stmt = SkipStmt
          | AssStmt String Exp
          | SeqStmt Stmt Stmt
          | IfStmt BExp Stmt Stmt
          | WhileStmt BExp Stmt
          | BlockStmt VDecl Stmt

-- AUXILIARY DOMAINS --

type Loc = Int
type Store = Map Loc Int
type VEnv = Map String Loc 

I

-- SEMANTIC FUNCTIONS --

semE :: Exp -> VEnv -> Store -> Int
semE (NumExp n) rho s = n
semE (VarExp v) rho s = s ! (rho ! v) 
semE (PlusExp e1 e2) rho s = (semE e1 rho s) + (semE e2 rho s)
semE (MinusExp e1 e2) rho s = (semE e1 rho s) - (semE e2 rho s)
semE (TimesExp e1 e2) rho s = (semE e1 rho s) * (semE e2 rho s)

semB :: BExp -> VEnv -> Store -> Bool
semB TrueBExp rho s = True
semB FalseBExp rho s = False
semB (LEqBExp e1 e2) rho s = (semE e1 rho s) <= (semE e2 rho s)
semB (NotBExp b) rho s = not (semB b rho s)
semB (AndBExp b1 b2) rho s = (semB b1 rho s) && (semB b2 rho s)

-- this not quite the same as in the lecture's slides:
-- newly declared variables are initialized to zero.
semD :: VDecl -> VEnv -> Store -> (VEnv,Store)
semD NoVDecl rho s = (rho,s)
semD (SingleVDecl v d) rho s = 
        let l = newloc s 
        in semD d (insert v l rho) (insert l 0 s)

semS :: Stmt -> VEnv -> Store -> Store
semS SkipStmt rho s = s
semS (AssStmt v e) rho s = 
       let n = semE e rho s in
       insert (rho ! v) n s
semS (SeqStmt i1 i2) rho s = 
       semS i2 rho (semS i1 rho s)
semS (IfStmt b i1 i2) rho s = 
       if semB b rho s
       then semS i1 rho s
       else semS i2 rho s
semS (WhileStmt b i) rho s =
       if semB b rho s
       then let s' = semS i rho s
            in semS (WhileStmt b i) rho s'
       else s
semS (BlockStmt vd i) rho s = 
       let (rho1,s1) = semD vd rho s
       in semS i rho1 s1

-- EXAMPLE: BASIC SCOPING --

{-- 
begin 
  var x;
  var y
  begin
    var x
    x := 7;
    y := x+1
  end;
  x := y+1
end
--}


fooStmt :: Stmt
fooStmt = 
  BlockStmt 
    (SingleVDecl "x" (SingleVDecl "y" NoVDecl))
    (SeqStmt
      (BlockStmt
        (SingleVDecl "x" NoVDecl)
        (SeqStmt
          (AssStmt "x" (NumExp 7))   
          (AssStmt "y" (PlusExp (VarExp "x") (NumExp 1)))
        )
      )
      (AssStmt "x" (PlusExp (VarExp "y") (NumExp 1)))
    )
 
{-- 
After running this program, locations 0 to 2 will hold values 9,8,7.

An example session that checks this: 
 $ghci
 GHCi, version 7.10.3: 
 Prelude> :load Declarations.hs
 [1 of 1] Compiling Main             ( Declarations.hs, interpreted )
 Ok, modules loaded: Main.
 *Main> let s = semS fooStmt empty empty
 *Main> s ! 0
 9
 *Main> s ! 1
 8
 *Main> s ! 2
 7
--}
