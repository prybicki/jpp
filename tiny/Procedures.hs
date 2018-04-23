{--------------------------------------------------
 Semantyka denotacyjna jezyka TINY 
 z bezparametrowymi procedurami rekurencyjnymi
 i statyczną widocznością identyfikatorów.
---------------------------------------------------}

{----------------------------------------------
 Semantyka zapisana w jezyku Haskell.
 
 Po uruchomieniu interpretera Haskella (ghci) 
 nalezy zaladowac plik Procedures.hs.
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

data Decl = NoDecl
           | SingleVDecl String Decl
           | SinglePDecl String Stmt Decl

data Stmt = SkipStmt
          | AssStmt String Exp
          | SeqStmt Stmt Stmt
          | IfStmt BExp Stmt Stmt
          | WhileStmt BExp Stmt
          | BlockStmt Decl Stmt
          | CallStmt String

-- AUXILIARY DOMAINS --

type Loc = Int
type Store = Map Loc Int
type VEnv = Map String Loc 
type Proc = Store -> Store
type PEnv = Map String Proc

--auxiliary function
newloc :: Store -> Loc
newloc s = if (Data.Map.null s) 
           then 0 
           else fst(findMax s) + 1

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
semD :: Decl -> VEnv -> PEnv -> Store -> (VEnv,PEnv,Store)
semD NoDecl rho pi s = (rho,pi,s)
semD (SingleVDecl v d) rho pi s = 
        let l = newloc s 
        in semD d (insert v l rho) pi (insert l 0 s)
semD (SinglePDecl p i d) rho pi s = 
        let pi1 = insert p (semS i rho pi1) pi
        in semD d rho pi1 s

semS :: Stmt -> VEnv -> PEnv -> Store -> Store
semS SkipStmt rho pi s = s
semS (AssStmt v e) rho pi s = 
       let n = semE e rho s in
       insert (rho ! v) n s
semS (SeqStmt i1 i2) rho pi s = 
       semS i2 rho pi (semS i1 rho pi s)
semS (IfStmt b i1 i2) rho pi s = 
       if semB b rho s
       then semS i1 rho pi s
       else semS i2 rho pi s
semS (WhileStmt b i) rho pi s =
       if semB b rho s
       then let s' = semS i rho pi s
            in semS (WhileStmt b i) rho pi s'
       else s
semS (BlockStmt vd i) rho pi s = 
       let (rho1,pi1,s1) = semD vd rho pi s
       in semS i rho1 pi1 s1
so

