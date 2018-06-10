module StaticCheck where

import AbsLatte
import PrintLatte

import Common
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad
import Data.Map as Map hiding(map, foldl)

-- Type of types environment.
-- Values of this maps are not lists which is one of the drawbacks atm.
type TEnv = Map Ident Type

-- Monadic type to represent static check computations.
type Check a = ReaderT Type (StateT TEnv (Except ErrorMsg)) a

-- Drawbacks of static checker to be solved in future:
-- Identifiers can share name *only* as variables or parameters in different functions.
-- invalid: int f(int f) {}
-- invalid: int f() { int f; }
-- invalid: int f() {} int g(int f) {}
-- invalid: int f() {} int g() { int f; }
-- valid:   int f(int a) { int p; } int g(int a) { int p; }
-- valid:   int f(int a) { int p; } int g(int p) { int a; }

-- Guarantees from static checker:
-- → All identifiers in any scope are unique and has unequivocal type
--   → In particular, there are no topdefs colliding with builtins
-- → All returns have correct type (as enclosing function return type)
-- → Expressions are well-typed
-- → All referenced variables exist in scope
-- → All called functions exist in scope

-- Main entry to static checker. Program topdefs should include builtins.
runStaticCheck :: Program -> Either ErrorMsg ()
runStaticCheck program =
  case runExcept (runStateT (runReaderT (checkProgram program) Void) emptyTEnv) of
    Right _ -> Right ()
    Left errMsg -> Left errMsg

checkProgram :: Program -> Check ()
checkProgram (Program topdefs) =
  do checkFunctionDeclarations topdefs
     checkFunctionDefinitions topdefs

-- -- -- Some helper functions -- -- --

emptyTEnv :: TEnv
emptyTEnv = Map.empty

tryAddToTEnv :: Ident -> Type -> ErrorMsg -> Check ()
tryAddToTEnv ident declType errMsg =
  do tenv <- get
     if Map.member ident tenv
     then throwError errMsg
     else modify (Map.insert ident declType)

forceAddToTEnv :: Ident -> Type -> Check ()
forceAddToTEnv ident declType =
   do tenv <- get
      modify (Map.insert ident declType)

-- -- -- Checking global definitions -- -- --

-- Declared function names and builtin names are unique.
-- Main do exist and have signature int main ()
-- Declared functions are added to type environment.
checkFunctionDeclarations :: [TopDef] -> Check ()
checkFunctionDeclarations topdefs =
  do foldM (\() -> checkFunctionDeclaration) () topdefs
     checkMainDeclaration
     where
       checkFunctionDeclaration :: TopDef -> Check ()
       checkFunctionDeclaration topdef =
         do tenv <- get
            case topdef of -- TODO Fix it?
              (FnDef declType ident args _) -> tryAddToTEnv ident (Fun declType (argTypes args)) ("Function identifier is already in use: " ++ (printTree (Builtin declType ident args)))
              (Builtin declType ident args) -> tryAddToTEnv ident (Fun declType (argTypes args)) ("Function identifier is already in use: " ++ (printTree (Builtin declType ident args)))
       checkMainDeclaration :: Check()
       checkMainDeclaration =
         do tenv <- get
            mainType <- tryFromJust (Map.lookup (Ident "main") tenv) "Missing main declaration"
            case mainType of
              Fun Int [] -> return ()
              x -> throwError ("Incorrect main signature, expected int (), found: " ++ (printTree x))

-- Argument names are unique (among any identifier, including functions).
-- Function body is correct.
-- Assumes builtins are correct.
checkFunctionDefinitions :: [TopDef] -> Check ()
checkFunctionDefinitions topdefs = foldM (\() -> checkFunctionDefinition) () topdefs
  where
   checkFunctionDefinition :: TopDef -> Check ()
   checkFunctionDefinition topdef =
     case topdef of
       Builtin _ _ _ -> return () -- TODO: Let's assume I did not fuck up.
       FnDef declType ident args block ->
         do oldState <- get
            addArgsToTEnv args
            local (const declType) (checkStmt (BStmt block))
            put oldState
     where
      addArgsToTEnv :: [Arg] -> Check ()
      addArgsToTEnv args = foldM (\() -> addArgToTenv) () args
      addArgToTenv :: Arg -> Check ()
      addArgToTenv (Arg declType ident) = tryAddToTEnv ident declType ("Function argument identifier is already in use" ++ (printTree (Arg declType ident)))

-- -- -- Checking statements -- -- --

checkTypesEqual :: Type -> Type -> ErrorMsg -> Check ()
checkTypesEqual t1 t2 errMsg =
  if t1 == t2
  then return ()
  else throwError errMsg

checkIdentHasType :: Ident -> Type -> ErrorMsg -> Check ()
checkIdentHasType ident expectedType errMsg =
  do tenv <- get
     foundType <- tryFromJust (Map.lookup ident tenv) ("Undeclared identifier " ++ (printTree ident))
     if foundType == expectedType
     then return ()
     else throwError errMsg

checkStmt :: Stmt -> Check ()
checkStmt stmt = case stmt of
  Empty -> return ()
  BStmt (Block stmts) -> foldM (\() -> checkStmt) () stmts
  Decl declType items -> foldM (\() -> checkDecl declType) () items
    where
      checkDecl :: Type -> Item -> Check ()
      checkDecl declType item =
        case item of
          NoInit ident -> tryAddToTEnv ident declType ("Duplicated declaration of local variable: " ++ (printTree ident))
          Init ident expr ->
            do exprType <- checkExprType expr
               checkTypesEqual declType exprType ("Invalid type of initializer expression: " ++ (printTree expr) ++ ", expected " ++ (printTree declType))
               tryAddToTEnv ident declType ("Duplicated declaration of local variable: " ++ (printTree ident))
  Ass ident expr -> do tenv <- get
                       exprType <- checkExprType expr
                       checkIdentHasType ident exprType ("Invalid expression type in assignment of local variable " ++ (printTree ident) ++ ": " ++ (printTree expr))
  Incr ident -> checkIdentHasType ident Int ("Expected integer variable in incrementation statement: " ++ (printTree (Incr ident)))
  Decr ident -> checkIdentHasType ident Int ("Expected integer variable in decrementation statement: " ++ (printTree (Incr ident)))
  Ret expr -> do expectedRetType <- ask
                 exprType <- checkExprType expr
                 checkTypesEqual exprType expectedRetType ("Invalid return type, expected " ++ (printTree expectedRetType) ++ ", in statement: " ++ printTree (Ret expr))
  VRet -> do expectedRetType <- ask
             checkTypesEqual Void expectedRetType ("Missing return expression in function returning " ++ (printTree expectedRetType))
  Cond expr stmt -> do exprType <- checkExprType expr
                       checkTypesEqual Bool exprType ("Expected bool in if condition: " ++ (printTree expr))
                       doAndDropState (checkStmt stmt)
  CondElse expr stmt1 stmt2 -> do exprType <- checkExprType expr
                                  checkTypesEqual Bool exprType ("Expected bool in if-else condition: "  ++ (printTree expr))
                                  doAndDropState (checkStmt stmt1)
                                  doAndDropState (checkStmt stmt2)
  While expr stmt -> do exprType <- checkExprType expr
                        doAndDropState (checkStmt stmt)
  For ident initExpr _ endExpr stmt -> do initType <- checkExprType initExpr
                                          endType <- checkExprType endExpr
                                          tryAddToTEnv ident initType ("Duplicated declaration of local variable: " ++ (printTree ident))
                                          checkTypesEqual Int initType ("Invalid start value type in for loop in expression: " ++ (printTree initExpr))
                                          checkTypesEqual Int endType ("Invalid end value type in for loop in expression: " ++ (printTree endExpr))
                                          doAndDropState (checkStmt stmt)
  SExp expr -> do checkExprType expr
                  return ()
  Break -> return ()
  Continue -> return ()

-- -- -- Checking expressions -- -- --

checkUnaryExprType :: Type -> Type -> Expr -> String -> Check Type
checkUnaryExprType expectedType exprType e errMsg =
  do tenv <- get
     eType <- checkExprType e
     if eType == expectedType
     then return exprType
     else throwError (errMsg ++ (printTree e))

checkBinaryExprType :: Type -> Type -> Type -> Expr -> Expr -> String -> Check Type
checkBinaryExprType expectedType1 expectedType2 exprType e1 e2 errMsg =
  do tenv <- get
     e1Type <- checkExprType e1
     e2Type <- checkExprType e2
     if (e1Type, e2Type) == (expectedType1, expectedType2)
     then return exprType
     else throwError (errMsg ++ (printTree e1) ++ " and " ++ (printTree e2))

checkExprType :: Expr -> Check Type
checkExprType expr =
  case expr of
    (EVar ident) -> do tenv <- get
                       tryFromJust (Map.lookup ident tenv) ("Local variable " ++ printTree ident ++ " is undeclared")
    (ELitInt _) -> return Int
    (ELitTrue) -> return Bool
    (ELitFalse) -> return Bool
    (EString _) -> return Str
    (Neg e) -> checkUnaryExprType Int Int e "Cannot negate (int) expression: "
    (Not e) -> checkUnaryExprType Bool Bool e "Cannot negate (bool) expression: "
    (EMul e1 op e2) -> checkBinaryExprType Int Int Int e1 e2 "Cannot multiply expressions: "
    (EAdd e1 Concat e2) -> checkBinaryExprType Str Str Str e1 e2 "Cannot concatenate expressions: "
    (EAdd e1 op e2) -> checkBinaryExprType Int Int Int e1 e2 "Cannot add expressions: "
    (ERel e1 STREQU e2) -> checkBinaryExprType Str Str Bool e1 e2 "Cannot compare strings: "
    (ERel e1 op e2) -> checkBinaryExprType Int Int Bool e1 e2 "Cannot compare expressions: "
    (EAnd e1 e2) -> checkBinaryExprType Bool Bool Bool e1 e2 "Cannot and expressions: "
    (EOr e1 e2) -> checkBinaryExprType Bool Bool Bool e1 e2 "Cannot or expressions: "
    -- Check number of arguments and their types.
    (EApp ident argExprs) ->
      do tenv <- get
         funType <- tryFromJust (Map.lookup ident tenv) ("Function " ++ (printTree ident) ++ " is undeclared")
         exprTypes <- mapM checkExprType argExprs
         case funType of
           (Fun declType argTypes) ->
             if not $ length exprTypes == length argTypes
             then throwError ("Invalid number of arguments in call: " ++ (printTree (EApp ident argExprs)))
             else if not $ argTypes == exprTypes
                  then throwError ("Invalid types in call: " ++ (printTree (EApp ident argExprs)))
                  else return declType
           _ -> throwError ("Cannot call " ++ (printTree ident) ++ ", it is not a function, in expression: " ++ (printTree (EApp ident argExprs)))
