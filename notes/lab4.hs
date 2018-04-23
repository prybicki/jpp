import Control.Monad.Reader
import Control.Monad.State
import Data.Map hiding(map)

allPairs :: [a] -> [a] -> [[a]]
-- allPairs xs ys = [[x, y] | x <- xs, y <- ys]

allPairs xs ys = do
   x <- xs
   y <- ys
   return [x, y]

allPairs2 xs ys = xs >>= (\x -> ys >>= (\y -> return [x, y]))

allPairs3 xs ys = concat (map (\x -> concat (map (\y -> [[x, y]]) ys)) xs)

allComb [] = [[]]
allComb (h:t) = do
    xt <- allComb t
    x <- h
    return (x:xt)


-- Zadanie 2
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

renumber :: Tree a -> Tree Int

renumberHelper :: Int -> Tree a -> Tree Int

renumberHelper d Empty = Empty
renumberHelper d (Node x l r) = Node d (renumberHelper (d + 1) l) (renumberHelper (d + 1) r)

renumber t = renumberHelper 0 t

renumberM t = renumber' t 0 where
    renumber' Empty = return Empty
    renumber' (Node _ l r) = do
        l' <- local (\d -> d + 1) $ renumber' l
        r' <- local (\d -> d + 1) $ renumber' r
        depth <- ask
        return (Node depth l' r')

-- Zadanie 2b
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

-- Zadanie 3
renumberTree :: Tree a -> Tree Int

renumberTree t = let (_, t') = _renum 0 t in t' where
    _renum n Empty = (n, Empty)
    _renum n (Node _ l r) = let (m, l') = _renum n l in let (k, r') = _renum (m + 1) r in (k, Node (m + 1) l' r')


renumberTreeM :: Tree a -> Tree Int
renumberTreeM t = evalState (renumberTree' t) 1
    where
        renumberTree' Empty = return Empty
        renumberTree' (Node a l r ) = do
            l' <- renumberTree' l
            modify (+1)
            s <- get
            r' <- renumberTree' r
            return (Node s l' r')


-- TINY
data Stmt = SSkip | SAsgn String Exp | SSeq Stmt Stmt | SIfte Exp Stmt Stmt | SWhile Exp Stmt

execStmt s = execState (_exec s) Data.Map.empty where
    _evalExp :: Exp -> Map String Int -> Int
    _evalExp (EInt n) = return n
    _evalExp (EOp o e1 e2) = do
        n1 <- _evalExp e1
        n2 <- _evalExp e2
        case o of
            OpAdd -> return $ n1 + n2
            otherwise -> return n1
    _evalExp (EVar x) = do
        e <- ask
        case Data.Map.lookup x e of
            Nothing -> return 0
            Just n -> return n
    _evalExp (ELet x e1 e2) = do
        n <- _evalExp e1
        m <- local (\e -> insert x n e) $ _evalExp e2
        return m
    _exec SSkip = return ()
    _exec (SSeq s1 s2) = do
        _exec s1
        _exec s2
    _exec (SAsgn x e) = do
        s <- Control.Monad.State.get
        let v = _evalExp e s
        modify $ insert x v
    _exec (SIfte e s1 s2) = do
        s <- Control.Monad.State.get
        let v = _evalExp e s
        if v /= 0 then _exec s1 else _exec s2

--  execStmt i = execState (execStmt' i) empty where
--      _evalExp :: Exp -> Map String Int -> Int
--      _evalExp (EInt n) = return n
--      _evalExp (EOp o e1 e2) = do
--          n <- _evalExp e1
--          m <- _evalExp e2
--          case o of
--              OpAdd -> return $ m + n
--              OpMul -> return $ m * n
--              OpSub -> return $ m - n
--      _evalExp (EVar x ) = do
--          e <- ask
--          case e !? x of
--              Nothing -> return 0
--              Just n -> return n
--      _evalExp (ELet x e1 e2) = do
--          n <- _evalExp e1
--          m <- local (\e -> insert x n e) $ _evalExp e2
--          return m
--      execStmt' SSkip = return ()
--      execStmt' (SAsgn x e) = do 
--          s <- get
--          let v = _evalExp e s
--          modify $ insert x v
--      execStmt' (SSeq i1 i2) = do
--          execStmt' i1
--          execStmt' i2
--      execStmt' (SIfte e i1 i2) = do
--          s <- get
--          let v = _evalExp e s
--          if v /= 0 then execStmt' i1 else execStmt' i2
--      execStmt' (SWhile e i) = do
--          s <- get
--          let v = _evalExp e s
--          if v /= 0 then do
--              execStmt' i
--              execStmt' (SWhile e i)
--          else return ()
        
        
