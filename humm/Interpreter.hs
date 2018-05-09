module Main where

import AbsLatte
import LexLatte
import ParLatte
import PrintLatte
import ErrM

import StaticCheck
import Common

import Data.Map as Map hiding(map,foldl)
import Data.List
import Data.Either as Either
import System.Environment
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Data.IORef
import Data.Maybe

-- Guarantees from static checker:
-- → All identifiers in any scope are unique and are one of Type
-- → All returns have matching type to declared
-- → Expressions have correct typing

data VarValue
  = VVoid
  | VInt Integer
  | VBoolean Bool
  | VString String
  deriving (Show, Eq, Ord)

type FEnv = Map Ident TopDef
type VEnv = Map Ident (IORef VarValue)
type Execution a = ReaderT FEnv (StateT VEnv (ExceptT String IO)) a

--------------------------------------------------------------------------------

makeProgram :: String -> Either Error Program
makeProgram s = case pProgram (myLexer s) of
  Bad err -> throwError err
  Ok e -> return e


-- execStmt :: Integer -> Execution VarValue
-- execStmt stmt
--   | stmt < 5 = do iovar <- liftIO $ newIORef (VInt stmt)
--                   modify (Map.insert (Ident "var") iovar)
--                   liftIO $ putStrLn "added var"
--                   liftIO $ putStrLn $ show (foldl (+) 0 [1..10000])
--                   throwError ("meh")

defaultVarValue :: Type -> VarValue
defaultVarValue declType = case declType of
  Int -> VInt 0
  Str -> VString ""
  Bool -> VBoolean False
  _ -> error("it should never happen ;)")

evalExpr :: Expr -> Execution VarValue
evalExpr expr = case expr of
  EVar ident -> do state <- get
                   return $ fromJust $ Map.lookup ident state

execStmt :: Stmt -> Execution VarValue
execStmt stmt = case stmt of
  Empty -> return VVoid
  Decl declType [] -> return VVoid
  Decl declType (item:t) -> case item of
    NoInit ident -> do ioValue <- liftIO $ newIORef (defaultVarValue declType)
                       modify (Map.insert ident ioValue)
                       return VVoid
    Init ident expr -> do fenv <- ask
                          state <- get
                          exprValue <- runExceptT (runStateT (runReaderT (evalExpr expr) fenv) state)
                          ioValue <- liftIO $ newIORef exprValue
                          modify (Map.insert ident ioValue)


makeFEnv :: Program -> FEnv
makeFEnv (Program topdefs) = Map.fromList (zip (map funIdent topdefs) topdefs)


main = do
  args <- getArgs
  fileStr <- readFile $ args !! 0
  let eitherProgram = makeProgram fileStr
  case eitherProgram of
    Left err -> putStrLn err
    Right program ->
      case (checkProgram program) of
        Left err -> putStrLn err
        Right () -> case program of
          (Program []) -> error("static")
          (Program lst) -> let Just (FnDef _ _ _ block) = find (\(FnDef _ ident _ _) -> ident == (Ident "main")) lst
                           in do a <- runExceptT (runStateT (runReaderT (execStmt (BStmt block)) (makeFEnv (Program lst))) (Map.fromList []))
                                 print ("OK")


-- main = do
--   a <- runExceptT (runStateT (runReaderT (execStmt 1) (Map.fromList [])) (Map.fromList []))
--   case a of
--     Left err -> print "error"
--     Right (value, state) -> print ("ok")

-- execStmt :: Int -> Execution Int
-- execStmt stmt
--   | stmt > 0 = do liftIO $ putStrLn "abc"
--                   return (stmt+1)
--   | stmt <= 0 = do throwError "def"
--
-- main = do a <- runExceptT $ execStmt (-3)
--           case a of
--             Left _ -> putStrLn "fail"
--             Right r -> putStrLn $ "ok: " ++ (show r)

-- main = execStmt 10




--
-- makePEnv :: [TopDef] -> PEnv
-- makePEnv topdefs = Map.fromList $ zip (map fnIdent topdefs) (topdefs)
--


-- checkTypes :: Program -> VarValue
-- checkTypes (Program topdefs) =
--   let penv = makePEnv topdefs
--   in checkTypes

-- interpret :: Program -> VarValue
-- interpret (Program topdefs) =
--   let penv = makePEnv topdefs
--   in evalExpr penv (Map.fromList []) (Map.fromList []) (EApp (Ident "main") [])
--
-- main = do
--   interact latte
--   putStrLn ""
--
-- latte s =
--   case pProgram (myLexer s) of
--     Ok e -> printTree (interpret e)
--     Bad s -> printTree s


--
-- evalExpr :: PEnv -> VEnv -> State -> Expr -> VarValue
-- evalExpr penv venv state (ELitInt i) = VInt i
-- evalExpr penv venv state ELitTrue = VBoolean True
-- evalExpr penv venv state ELitFalse = VBoolean False
-- evalExpr penv venv state (EString str) = VString str
-- evalExpr penv venv state (EMul e1 Times e2) = let VInt n1 = evalExpr penv venv state e1
--                                                   VInt n2 = evalExpr penv venv state e2
--                                                   in VInt (n1 * n2)
--
-- evalExpr penv venv state (EApp ident args) =
--   let maybeTopdef = (Map.lookup ident penv)
--   in case maybeTopdef of
--      Nothing -> error("Function " ++ (printTree ident) ++ " is not in scope")
--      Just (FnDef t i args block) -> let (_, _, maybeReturn) = evalStmt penv venv state (BStmt block)
--                                    in case maybeReturn of
--                                       Nothing -> error("Function " ++ printTree ident ++ " did not returned any value")
--                                       Just VVoid -> error("Ooops - ta funkcja nie powinna byc w tym miejscu")
--                                       Just value -> value
--
-- evalStmt :: PEnv -> VEnv -> State -> Stmt -> (VEnv, State, Maybe VarValue)
-- evalStmt penv venv state stmt = case stmt of
--   Empty -> (venv, state, Nothing)
--   VRet -> (venv, state, Just VVoid)
--   Ret e -> (venv, state, Just (evalExpr penv venv state e))
--   BStmt (Block []) -> (venv, state, Nothing)
--   BStmt (Block (h:t)) -> let (venv', state', v) = evalStmt penv venv state h
--                          in case v of
--                             Nothing -> evalStmt penv venv' state' (BStmt (Block t))
--                             Just value -> (venv', state', (Just value))
