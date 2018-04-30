import Control.Monad.Reader
import Control.Monad.State
import Data.Map hiding(map)

type Var = String
data Exp = EInt Int
     | EOp  Op Exp Exp
     | EVar Var
     | ELet Var Exp Exp  -- let var = e1 in e2
     deriving (Show, Eq)

data Op = OpAdd | OpMul | OpSub deriving (Show, Eq)

test = ELet "x" (ELet "y" (EOp OpAdd (EInt 6) (EInt 9))
                      (EOp OpSub y (EInt 1)))
                (EOp OpMul x (EInt 3))
    where x = EVar "x"
          y = EVar "y"

evalExp e = _evalExp e (fromList []) where
    _evalExp :: Exp -> Map String Int -> Int
    _evalExp (EInt n) = return n
    _evalExp (EOp o e1 e2) = do
        n <- _evalExp e1
        m <- _evalExp e2
        case o of
            OpAdd -> return $ m + n
            OpMul -> return $ m * n
            OpSub -> return $ m - n
    _evalExp (EVar x ) = do
        e <- ask
        case e !? x of
            Nothing -> return 0
            Just n -> return n
    _evalExp (ELet x e1 e2) = do
        n <- _evalExp e1
        m <- local (\e -> insert x n e) $ _evalExp e2
        return m
