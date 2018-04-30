import Control.Monad.State
import Data.Map as Map
import Data.Maybe
import Control.Monad
import Control.Monad.Reader

type Var = String
data Exp = EInt Int
     | EOp  Op Exp Exp
     | EVar Var
     | ELet Var Exp Exp  -- let var = e1 in e2
    deriving Show

data BExp = BLte Exp Exp
        | BNEq Exp Exp

data Stmt = SSkip
                | SSet Var Exp
                | SIf BExp Stmt Stmt
                | SWhile BExp Stmt
                | S Stmt Stmt

data Op = OpAdd | OpMul | OpSub deriving Show

func OpAdd = (+)
func OpMul = (*)
func OpSub = (-)

test = ELet "x" (ELet "y" (EOp OpAdd (EInt 6) (EInt 9))
                      (EOp OpSub y (EInt 1)))
                (EOp OpMul x (EInt 3))
    where x = EVar "x"
          y = EVar "y"

test2 = ELet "y" (EInt 5) (EVar "y")

type MyEnv = Map.Map Var Int
type ExprReader = Reader MyEnv Int

evalExp' :: Exp -> ExprReader
evalExp' (EVar v) = do
        env <- ask
        return $ fromMaybe (error ("Blad! nie ma zmiennej " ++ v) 0) (Map.lookup v env)

evalExp' (EInt a) = return a

evalExp' (ELet v e1 e2) = do
        eval <- (evalExp' e1)
        local (Map.insert v eval) (evalExp' e2)

evalExp' (EOp op e1 e2) = liftM2 (func op) (evalExp' e1) (evalExp' e2)

evalExp :: Exp -> Int
evalExp x = runReader (evalExp' x) Map.empty

evalExp2 x s = runReader (evalExp' x) s

-- bexpr
type BExprReader = Reader MyEnv Bool

evalBExp' :: BExp -> BExprReader
evalBExp' (BLte e1 e2) = do
        env <- ask
        let r1 = evalExp2 e1 env
        let r2 = evalExp2 e2 env
        return $ r1 < r2
evalBExp' (BNEq e1 e2) = do
        env <- ask
        let r1 = evalExp2 e1 env
        let r2 = evalExp2 e2 env
        return $ r1 /= r2
evalBExp x s = runReader (evalBExp' x) s



type StmtState = State MyEnv ()

exec :: Stmt -> StmtState
exec SSkip = return ()
exec (SSet v e) = do
        res <- gets (evalExp2 e)
        modify (Map.insert v res)
        return ()

exec (S s1 s2) = exec s1 >> exec s2

exec (SIf b s1 s2) = do
        res <- gets (evalBExp b)
        if res then exec s1 else exec s2

exec (SWhile b s) = exec (SIf b (S (s) (SWhile b s)) (SSkip))

runExec s = snd $ runState (exec s) Map.empty

program2 = S (SSet "y" (EInt 10)) (S (SSet "x" (EInt 0)) (SWhile (BLte (EVar "x") (EInt 10)) (S (SSet "x" (EOp OpAdd (EVar "x") (EInt 1))) (SSet "y" (EOp OpSub (EVar "y") (EInt 1))))))

program = S (S (S (SSet "v" (EInt 5)) (SSet "v" (EOp OpAdd (EVar "v") (EInt 1)))) (SSet "y" (EInt 10))) (SIf (BLte (EVar "v") (EVar "y")) (SSet "x" (EInt 1)) (SSet "x" (EInt 2)))


testFib :: Stmt
testFib =
  S
    (SSet "x" (EInt 0))
    (S
       (SSet "y" (EInt 1))
       (S
          (SSet "i" (EInt 0))
          (SWhile
             (BNEq (EInt 0) (EOp OpSub (EVar "i") (EVar "n")))
             (S
                (SSet "tmp" (EOp OpAdd (EVar "x") (EVar "y")))
                (S
                   (SSet "x" (EVar "y"))
                   (S
                     (SSet "y" (EVar "tmp"))
                     (SSet "i" (EOp OpAdd (EVar "i") (EInt 1)))
                   )
                )
             )
          )
       )
    )

fib :: Int -> Int
fib n = runExec (S (SSet "n" (EInt n)) testFib) ! "x"
