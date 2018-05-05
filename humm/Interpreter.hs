module Main where

import AbsLatte
import LexLatte
import ParLatte
import PrintLatte
import ErrM

import Data.Map as Map hiding(map,foldl)
import Data.Either as Either
import System.Environment

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

type TEnv = Map Ident Type
type Error = String

checkUnaryExprType :: TEnv -> Type -> Type -> Expr -> String -> Either Error Type
checkUnaryExprType tenv tIn1 tOut e errMsg =
  do eType <- checkExprType tenv e
     if eType == tIn1
     then Right tOut
     else Left (errMsg ++ (printTree e))

checkBinaryExprType :: TEnv -> Type -> Type -> Type -> Expr -> Expr -> String -> Either Error Type
checkBinaryExprType tenv tIn1 tIn2 tOut e1 e2 errMsg =
  do e1Type <- checkExprType tenv e1
     e2Type <- checkExprType tenv e2
     if (e1Type, e2Type) == (tIn1, tIn2)
     then Right tOut
     else Left (errMsg ++ (printTree e1) ++ " and " ++ (printTree e2))

checkExprType :: TEnv -> Expr -> Either Error Type
checkExprType tenv expr = case expr of
  (EVar ident) -> case (Map.lookup ident tenv) of
                    Nothing -> Left ("Variable " ++ printTree ident ++ " is undeclared")
                    Just t -> Right t
  (ELitInt _) -> Right Int
  (ELitTrue) -> Right Bool
  (ELitFalse) -> Right Bool
  (EString _) -> Right Str
  (Neg e) -> checkUnaryExprType tenv Int Int e "Cannot negate (int) expression: "
  (Not e) -> checkUnaryExprType tenv Bool Bool e "Cannot negate (bool) expression: "
  (EMul e1 op e2) -> checkBinaryExprType tenv Int Int Int e1 e2 "Cannot multiply expressions: "
  (EAdd e1 op e2) -> checkBinaryExprType tenv Int Int Int e1 e2 "Cannot add expressions: "
  (ERel e1 op e2) -> checkBinaryExprType tenv Int Int Bool e1 e2 "Cannot compare expressions: "
  (EAnd e1 e2) -> checkBinaryExprType tenv Bool Bool Bool e1 e2 "Cannot and expressions: "
  (EOr e1 e2) -> checkBinaryExprType tenv Bool Bool Bool e1 e2 "Cannot or expressions: "
  (EApp ident argExprs) -> case (Map.lookup ident tenv) of
    Nothing -> Left ("Function " ++ printTree ident ++ " is undeclared")
    Just (Fun declType argTypes) ->
     let exprTypes = rights (map (checkExprType tenv) argExprs)
     in if not $ length exprTypes == length argTypes
        then Left ("Invalid number of arguments in call: " ++ (printTree (EApp ident argExprs)))
        else if not $ argTypes == exprTypes
             then Left ("Invalid types in call: " ++ (printTree (EApp ident argExprs))) -- Todo printTree which arg is bad
             else Right declType
    _ -> Left ("Cannot call " ++ (printTree ident) ++ ", it is not a function")
-- -- --
--
checkItemType :: TEnv -> Type -> Item -> Either Error TEnv
checkItemType tenv declType item = case item of
  NoInit ident -> Right (Map.insert ident declType tenv)
  Init ident expr -> do
                       exprType <- checkExprType tenv expr
                       if declType == exprType
                       then Right (Map.insert ident declType tenv)
                       else Left ("Invalid type of initializer expression: " ++ (printTree expr) ++ ", expected " ++ (printTree declType))
--
checkIdentHasType :: TEnv -> Type -> Ident -> Either Error ()
checkIdentHasType tenv expectedType ident =
  case (Map.lookup ident tenv) of
    Nothing -> Left ("Undeclared identifier " ++ (printTree ident))
    Just foundType -> if foundType == expectedType
                      then Right ()
                      else Left ("Invalid type, expected expression of type " ++ (printTree foundType) ++ " to assign " ++ (printTree ident))

-- -- TODO: Poprawić to że tu się nic nie dzieje ;O
-- -- Potem dodać either
checkStmt :: TEnv -> Type -> Stmt -> Either Error TEnv
checkStmt tenv expectedRetType stmt = case stmt of
  Empty -> Right tenv
  BStmt (Block stmts) -> case stmts of
                         [] -> Right tenv
                         (h:t) -> do tenv' <- checkStmt tenv expectedRetType h
                                     checkStmt tenv' expectedRetType (BStmt (Block t))
  Decl declType items -> case items of
                         [] -> Right tenv
                         (h:t) -> do tenv' <- checkItemType tenv declType h
                                     checkStmt tenv' expectedRetType (Decl declType t)
  Ass ident expr -> do exprType <- checkExprType tenv expr
                       checkIdentHasType tenv exprType ident
                       return tenv
  Incr ident -> do {checkIdentHasType tenv Int ident; return tenv}
  Decr ident -> do {checkIdentHasType tenv Int ident; return tenv}
  Ret expr -> do exprType <- checkExprType tenv expr
                 if exprType == expectedRetType
                 then Right tenv
                 else Left ("Invalid return type, expected " ++ (printTree expectedRetType) ++ ", in statement: " ++ printTree (Ret expr))
  VRet -> if expectedRetType == Void
          then Right tenv
          else Left ("Missing return expression in function returning " ++ (printTree expectedRetType))
  Cond expr stmt -> do exprType <- checkExprType tenv expr
                       if Bool == exprType
                       then checkStmt tenv expectedRetType stmt
                       else Left ("Expected bool in if/while condition: " ++ (printTree expr))
                       return tenv
  CondElse expr stmt1 stmt2 -> do checkStmt tenv expectedRetType (Cond expr stmt1)
                                  checkStmt tenv expectedRetType stmt2
  While expr stmt -> checkStmt tenv expectedRetType (Cond expr stmt)
  For _ initExpr _ endExpr stmt -> do initType <- checkExprType tenv initExpr
                                      endType <- checkExprType tenv endExpr
                                      if initType == Int && endType == Int
                                      then do checkStmt tenv expectedRetType stmt
                                              return tenv
                                      else Left ("Invalid initial or end value of for loop: " ++ (printTree initExpr) ++ " or " ++ (printTree endExpr))
  SExp expr -> do checkExprType tenv expr
                  return tenv
--
argType :: Arg -> Type
argType (Arg declType _) = declType

funIdent :: TopDef -> Ident
funIdent (FnDef _ ident _ _) = ident

funType :: TopDef -> Type
funType (FnDef declType _ args _) = Fun declType (map argType args)

addArgsToTEnv :: TEnv -> [Arg] -> TEnv
addArgsToTEnv tenv args = foldl (\env (Arg declType ident) -> Map.insert ident declType env) tenv args

checkTopdefs :: TEnv -> [TopDef] -> Either Error ()
checkTopdefs tenv topdefs = case topdefs of
  [] -> return ()
  ((FnDef declType ident args block):t) ->
    do
    let tenv' = addArgsToTEnv tenv args
    checkStmt tenv' declType (BStmt block)
    checkTopdefs tenv t

checkProgram :: Program -> Either Error ()
checkProgram (Program topdefs) =
  let tenv = (Map.fromList $ zip (map funIdent topdefs) (map funType topdefs))
  in do checkTopdefs tenv topdefs
        return ()

makeProgram :: String -> Either Error Program
makeProgram s = case pProgram (myLexer s) of
  Bad s -> Left s
  Ok e -> Right e

main = do
  args <- getArgs
  fileStr <- readFile $ args !! 0
  let eitherProgram = makeProgram fileStr
  case eitherProgram of
    Left err -> putStrLn err
    Right program ->
      case (checkProgram program) of
        Left err -> putStrLn err
        Right () -> putStrLn "OK"


-- Sprawdzenie typów funkcji:
-- W wywołaniach zakładam, że funkcja ma typ zgodny z deklaracją
-- Natomiast w definicji sprawdzam czy każdy return zwraca deklarowany typ
-- W przypadku gdy pewna ścieżka nie kończy się return-em funkcja
-- zwraca domyślną wartość dla zwracanego typu i generowane jest ostrzeżenie.

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
