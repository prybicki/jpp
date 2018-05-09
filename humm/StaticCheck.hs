module StaticCheck where

import AbsLatte
import PrintLatte

import Common
import Data.List
import Data.Either as Either
import Data.Map as Map hiding(map,foldl)

-- TODO: Make monads great here.

-- Sprawdzenie typów funkcji:
-- W wywołaniach zakładam, że funkcja ma typ zgodny z deklaracją
-- Natomiast w definicji sprawdzam czy każdy return zwraca deklarowany typ
-- W przypadku gdy pewna ścieżka nie kończy się return-em funkcja
-- zwraca domyślną wartość dla zwracanego typu i generowane jest ostrzeżenie.

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


-- TODO Remove duplicated code :'(
checkItemType :: TEnv -> Type -> Item -> Either Error TEnv
checkItemType tenv declType item = case item of
  NoInit ident -> if Map.member ident tenv
                  then Left ("Double declaration of " ++ (printTree ident))
                  else Right (Map.insert ident declType tenv)
  Init ident expr -> if Map.member ident tenv
                     then Left ("Double declaration of " ++ (printTree ident))
                     else do exprType <- checkExprType tenv expr
                             if declType == exprType
                             then Right (Map.insert ident declType tenv)
                             else Left ("Invalid type of initializer expression: " ++ (printTree expr) ++ ", expected " ++ (printTree declType))

checkIdentHasType :: TEnv -> Type -> Ident -> Either Error ()
checkIdentHasType tenv expectedType ident =
  case (Map.lookup ident tenv) of
    Nothing -> Left ("Undeclared identifier " ++ (printTree ident))
    Just foundType -> if foundType == expectedType
                      then Right ()
                      else Left ("Invalid type, expected expression of type " ++ (printTree foundType) ++ " to assign " ++ (printTree ident))

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

addIdentToTEnv :: TEnv -> (Ident, Type) -> Either Error TEnv
addIdentToTEnv tenv (ident, declType) = if Map.notMember ident tenv
                                     then Right (Map.insert ident declType tenv)
                                     else Left ("duplicated identifier " ++ (printTree ident))

addArgsToTEnv :: TEnv -> [Arg] -> Either Error TEnv
addArgsToTEnv tenv args = case args of
  [] -> Right tenv
  ((Arg declType ident):t) -> do tenv' <- addIdentToTEnv tenv (ident, declType)
                                 addArgsToTEnv tenv' t

checkTopdefs :: TEnv -> [TopDef] -> Either Error ()
checkTopdefs tenv topdefs = case topdefs of
  [] -> Right ()
  ((FnDef declType ident args block):t) ->
    do tenvWithArgs <- addArgsToTEnv tenv args
       checkStmt tenvWithArgs declType (BStmt block)
       checkTopdefs tenv t

elementsUnique :: Ord a => [a] -> Bool
elementsUnique lst = length (group (sort lst)) == length lst

checkProgram :: Program -> Either Error ()
checkProgram (Program topdefs) =
  if not (elementsUnique (map funIdent topdefs))
  then Left ("function names are not unique")
  else let tenv = (Map.fromList $ zip (map funIdent topdefs) (map funType topdefs))
           maybeMain = Map.lookup (Ident "main") tenv
       in case maybeMain of
             Nothing -> Left ("Function main is not defined")
             Just (Fun declType args) ->
               if length args > 0
               then Left ("Function main should not have arguments")
               else if not (declType == Int)
                    then Left ("Function main should be declared as int")
                    else checkTopdefs tenv topdefs
