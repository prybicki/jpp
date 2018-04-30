module Interpreter where

import AbsLatte
import LexLatte
import ParLatte
import ErrM

import Data.Map as Map hiding(map)
import Data.IORef as IORef

fnIdent :: TopDef -> Ident
fnIdent (FnDef _ i _ _) = i


type VarLocation = Int
data VarValue
  = VVoid
  | VInt Integer
  | VBoolean Bool
  | VString String
  deriving (Show)

type PEnv = Map Ident TopDef
type VEnv = Map Ident VarLocation
type State = Map VarLocation VarValue

identToStr :: Ident -> String
identToStr (Ident str) = str

type TEnv = Map Ident Type

checkUnaryExprType :: TEnv -> Type -> Type -> Expr -> String -> Type
checkUnaryExprType tenv tIn1 tOut e errMsg =
  let eType = typeExpr tenv e
  in if eType == tIn1
     then tOut
     else error(errMsg ++ (show e))

checkBinaryExprType :: TEnv -> Type -> Type -> Type -> Expr -> Expr -> String -> Type
checkBinaryExprType tenv tIn1 tIn2 tOut e1 e2 errMsg =
  let e1Type = typeExpr tenv e1
      e2Type = typeExpr tenv e2
  in if (e1Type, e2Type) == (tIn1, tIn2)
     then tOut
     else error(errMsg ++ (show e1) ++ " and " ++ (show e2))

typeExpr :: TEnv -> Expr -> Type
typeExpr tenv expr = case expr of
  (EVar ident) -> let maybeType = (Map.lookup ident tenv)
                  in case maybeType of
                     Nothing -> error("Variable " ++ identToStr ident ++ " undeclared")
                     Just t -> t
  (ELitInt _) -> Int
  (ELitTrue) -> Bool
  (ELitFalse) -> Bool
  (EString _) -> Str
  (Neg e) -> checkUnaryExprType tenv Int Int e "Cannot negate (int) expression: "
  (Not e) -> checkUnaryExprType tenv Bool Bool e "Cannot negate (bool) expression: "
  (EMul e1 op e2) -> checkBinaryExprType tenv Int Int Int e1 e2 "Cannot multiply expressions: "
  (EAdd e1 op e2) -> checkBinaryExprType tenv Int Int Int e1 e2 "Cannot add expressions: "
  (ERel e1 op e2) -> checkBinaryExprType tenv Int Int Bool e1 e2 "Cannot compare expressions: "
  (EAnd e1 e2) -> checkBinaryExprType tenv Bool Bool Bool e1 e2 "Cannot and expressions: "
  (EOr e1 e2) -> checkBinaryExprType tenv Bool Bool Bool e1 e2 "Cannot or expressions: "
  (EApp ident args) -> error("Not implemented yet")


evalExpr :: PEnv -> VEnv -> State -> Expr -> VarValue
evalExpr penv venv state (ELitInt i) = VInt i
evalExpr penv venv state ELitTrue = VBoolean True
evalExpr penv venv state ELitFalse = VBoolean False
evalExpr penv venv state (EString str) = VString str
evalExpr penv venv state (EMul e1 Times e2) = let VInt n1 = evalExpr penv venv state e1
                                                  VInt n2 = evalExpr penv venv state e2
                                                  in VInt (n1 * n2)



evalExpr penv venv state (EApp ident args) =
  let maybeTopdef = (Map.lookup ident penv)
  in case maybeTopdef of
     Nothing -> error("Function " ++ (identToStr ident) ++ " is not in scope")
     Just (FnDef t i args block) -> let (_, _, maybeReturn) = evalStmt penv venv state (BStmt block)
                                   in case maybeReturn of
                                      Nothing -> error("Function " ++ identToStr ident ++ " did not returned any value")
                                      Just VVoid -> error("Ooops - ta funkcja nie powinna byc w tym miejscu")
                                      Just value -> value

evalStmt :: PEnv -> VEnv -> State -> Stmt -> (VEnv, State, Maybe VarValue)
evalStmt penv venv state stmt = case stmt of
  Empty -> (venv, state, Nothing)
  VRet -> (venv, state, Just VVoid)
  Ret e -> (venv, state, Just (evalExpr penv venv state e))
  BStmt (Block []) -> (venv, state, Nothing)
  BStmt (Block (h:t)) -> let (venv', state', v) = evalStmt penv venv state h
                         in case v of
                            Nothing -> evalStmt penv venv' state' (BStmt (Block t))
                            Just value -> (venv', state', (Just value))

makePEnv :: [TopDef] -> PEnv
makePEnv topdefs = Map.fromList $ zip (map fnIdent topdefs) (topdefs)

makeProgram :: String -> Program
makeProgram s = case pProgram (myLexer s) of
  Bad s -> Program []
  Ok e -> e

-- checkTypes :: Program -> VarValue
-- checkTypes (Program topdefs) =
--   let penv = makePEnv topdefs
--   in checkTypes

interpret :: Program -> VarValue
interpret (Program topdefs) =
  let penv = makePEnv topdefs
  in evalExpr penv (Map.fromList []) (Map.fromList []) (EApp (Ident "main") [])
--
-- main = do
--   interact latte
--   putStrLn ""
--
-- latte s =
--   case pProgram (myLexer s) of
--     Ok e -> show (interpret e)
--     Bad s -> show s
