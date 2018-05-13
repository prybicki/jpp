{-# LANGUAGE LambdaCase #-}

module Main where

import AbsLatte
import LexLatte
import ParLatte
import PrintLatte
import ErrM

import StaticCheck
import Common

import Data.List
import Data.IORef
import Data.Either as Either
import Data.Map as Map hiding(map,foldl)
import Text.Read hiding(Ident, get)
import System.Environment
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

-- See AbsLatte for AST definitions.

-- Types of available variables + return types.
data VarValue
  = VVoid
  | VInt Integer
  | VBoolean Bool
  | VString String
  deriving (Show, Eq, Ord)

-- Function environment
type FEnv = Map Ident TopDef

-- Variables environment, automatic garbage-collection thanks to IORefs
type MemState = Map Ident (IORef VarValue)

-- Exceptions to break normal flow of the program.
-- In future break and continue will be implemented using this mechanism.
data ExceptionType = ReturnException VarValue | ErrorException ErrorMsg

-- Monadic type to represent execution computations.
type Execution a = ReaderT FEnv (StateT MemState (ExceptT ExceptionType IO)) a

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  if not $ (length args) == 1
  then putStrLn "Usage: ./interpreter <path-to-program>"
  else do fileStr <- readFile $ args !! 0
          let maybeProgram = runExcept $ makeProgram fileStr
          case maybeProgram of
            Left err -> putStrLn err
            Right program -> execProgram program

makeProgram :: String -> Except ErrorMsg Program
makeProgram str =
  do (Program topdefs) <- parseProgram str
     let programWithBuiltins = (Program (builtins ++ topdefs))
     case runStaticCheck programWithBuiltins of
       Left errMsg -> throwError ("Static check error: " ++ errMsg)
       Right () -> return programWithBuiltins
  where
   parseProgram :: String -> Except ErrorMsg Program
   parseProgram str =
     case pProgram (myLexer str) of
       Bad errMsg -> throwError ("Parse error: " ++ errMsg)
       Ok parsedProgram -> return parsedProgram

execProgram :: Program -> IO ()
execProgram (Program topdefs) = do
  runResult <- runExceptT (runStateT (runReaderT (evalExpr startExpr) startingEnv) startingMemState)
  case runResult of
    Left (ErrorException errMsg) -> putStrLn ("Runtime error: " ++ errMsg)
    Left (ReturnException _) -> error("Return exception is not handled in evalExpr!")
    Right ((VInt value), memstate) -> putStrLn ("Return value: " ++ (show value))
    _ -> error("Main did not return int, most likely static analyzer fault ;(")
  where
    startExpr = (EApp (Ident "main") [])
    startingEnv = Map.fromList (((zip (map funIdent topdefs)) (topdefs ++ builtins)))
    startingMemState = Map.fromList []

-- Some general helper functions

typeDefaultValue :: Type -> VarValue
typeDefaultValue declType = case declType of
  Int -> VInt 0
  Str -> VString ""
  Bool -> VBoolean False

doAndDropStateWithState :: MonadState s m => s -> m a -> m ()
doAndDropStateWithState state action =
 do put state
    doAndDropState action

-- Expressions

evalExpr :: Expr -> Execution VarValue
evalExpr expr = case expr of
  EVar ident ->
    do state <- get
       liftIO $ readIORef $ forceFromJust (Map.lookup ident state) ("variable not found: " ++ (printTree ident))
  ELitInt n -> return (VInt n)
  ELitTrue -> return (VBoolean True)
  ELitFalse -> return (VBoolean False)
  EApp ident exprs ->
    do memstate <- get
       fenv <- ask
       let topdef = forceFromJust (Map.lookup ident fenv) ("topdef not found: " ++ (printTree ident))
       case topdef of
         (Builtin declType _ args) ->
           do values <- mapM evalExpr exprs
              ioValues <- mapM (liftIO . newIORef) values
              do {
                 doAndDropStateWithState (Map.fromList (zip (map argIdent args) ioValues)) (execBuiltin ident);
                 error ("Builtin did not returned any value ;(")
              }
              `catchError` (\case
                ReturnException varValue -> return varValue
                err -> throwError err)
         (FnDef declType _ args block) ->
           do values <- mapM evalExpr exprs
              ioValues <- mapM (liftIO . newIORef) values
              do {
                 doAndDropStateWithState (Map.fromList (zip (map argIdent args) ioValues)) (execStmt (BStmt block));
                 return (typeDefaultValue declType)
              }
              `catchError` (\case
                ReturnException varValue -> return varValue
                err -> throwError err)
  EString str -> return (VString str)
  Neg expr ->
    do (VInt value) <- evalExpr expr
       return (VInt (-value))
  Not expr ->
    do (VBoolean value) <- evalExpr expr
       return (VBoolean (not value))
  EMul expr1 op expr2 ->
    do
       (VInt value1) <- evalExpr expr1
       (VInt value2) <- evalExpr expr2
       case op of
         Times -> return (VInt (value1 * value2))
         Div -> if value2 == 0
                then throwError (ErrorException ("Division by zero in expression: " ++ (printTree (EMul expr1 op expr2))))
                else return (VInt (value1 `div` value2))
         Mod -> if value2 == 0
                then throwError (ErrorException ("Modulo by zero in expression: " ++ (printTree (EMul expr1 op expr2))))
                else return (VInt (value1 `mod` value2))
  EAdd expr1 Concat expr2 ->
    do (VString str1) <- evalExpr expr1
       (VString str2) <- evalExpr expr2
       return (VString (str1 ++ str2))
  EAdd expr1 op expr2 ->
    do (VInt value1) <- evalExpr expr1
       (VInt value2) <- evalExpr expr2
       case op of
         Plus -> return (VInt (value1 + value2))
         Minus -> return (VInt (value1 - value2))
  ERel expr1 STREQU expr2 ->
    do (VString str1) <- evalExpr expr1
       (VString str2) <- evalExpr expr2
       return (VBoolean (str1 == str2))
  ERel expr1 op expr2 ->
    do (VInt value1) <- evalExpr expr1
       (VInt value2) <- evalExpr expr2
       case op of
         LTH -> return (VBoolean (value1 < value2))
         LE  -> return (VBoolean (value1 <= value2))
         GTH -> return (VBoolean (value1 > value2))
         GE  -> return (VBoolean (value1 >= value2))
         EQU -> return (VBoolean (value1 == value2))
         NE  -> return (VBoolean (not (value1 == value2)))
  EAnd expr1 expr2 ->
    do (VBoolean value1) <- evalExpr expr1
       (VBoolean value2) <- evalExpr expr2
       return (VBoolean (value1 && value2))
  EOr expr1 expr2 ->
    do (VBoolean value1) <- evalExpr expr1
       (VBoolean value2) <- evalExpr expr2
       return (VBoolean (value1 || value2))

-- Builtins

execBuiltin :: Ident -> Execution VarValue
execBuiltin (Ident name) = case name of
  "print" -> -- void print(string text)
    do memstate <- get
       (VString text) <- liftIO $ readIORef $ forceFromJust (Map.lookup (Ident "text") memstate) "builtin argument not found: text"
       liftIO $ putStrLn text
       throwError (ReturnException VVoid)
  "intToStr" -> -- string intToStr(int number)
    do memstate <- get
       (VInt number) <- liftIO $ readIORef $ forceFromJust (Map.lookup (Ident "number") memstate) "builtin argument not found: number"
       throwError (ReturnException (VString (show number)))
  "strToInt" -> -- int strToInt (string text)
    do memstate <- get
       (VString text) <- liftIO $ readIORef $ forceFromJust (Map.lookup (Ident "text") memstate) "builtin argument not found: text"
       let maybeValue = readMaybe (text)
       case maybeValue of
         Nothing -> throwError (ErrorException ("Could not convert string to int: " ++ (show text)))
         Just value -> throwError (ReturnException (VInt value))
  _ -> error("Congratulations, you succeded to call non-existing builtin!")

-- Statements

execStmt :: Stmt -> Execution ()
execStmt stmt = case stmt of
  Empty -> return ()
  BStmt (Block stmts) -> mapM_ execStmt stmts
  Decl declType items -> foldM (\() -> execDecl declType) () items
    where
      execDecl :: Type -> Item -> Execution ()
      execDecl declType item = case item of
        NoInit ident -> do ioValue <- liftIO $ newIORef (typeDefaultValue declType)
                           modify (Map.insert ident ioValue)
        Init ident expr -> do exprValue <- evalExpr expr
                              ioValue <- liftIO $ newIORef exprValue
                              modify (Map.insert ident ioValue)
  Ass ident expr -> do value <- evalExpr expr
                       modifyVariable ident (const value)
  Incr ident -> modifyVariable ident (\(VInt n) -> (VInt (n+1)))
  Decr ident -> modifyVariable ident (\(VInt n) -> (VInt (n-1)))
  Ret expr -> do value <- evalExpr expr
                 throwError (ReturnException value)
  VRet -> throwError (ReturnException VVoid)
  Cond expr stmt ->
    do (VBoolean condition) <- evalExpr expr
       if not condition
       then do return ()
       else do doAndDropState (execStmt stmt)
  CondElse expr ifstmt elsestmt ->
    do (VBoolean condition) <- evalExpr expr
       if condition
       then do doAndDropState (execStmt ifstmt)
       else do doAndDropState (execStmt elsestmt)
  While expr stmt ->
    do (VBoolean condition) <- evalExpr expr
       if condition
       then do doAndDropState (execStmt stmt)
               execStmt (While expr stmt)
       else return ()
  For counterIdent startExpr forType stopExpr stmt ->
    do memstate <- get
       firstValue <- evalExpr startExpr
       lastValue <- evalExpr stopExpr
       ioValue <- liftIO $ newIORef firstValue
       modify (Map.insert counterIdent ioValue)
       doFor counterIdent forType lastValue stmt
       put memstate
  SExp expr -> do { (evalExpr expr); return () }
  where
   doFor :: Ident -> ForT -> VarValue -> Stmt -> Execution ()
   doFor counterIdent forType (VInt lastValue) stmt =
    do (VInt counterValue) <- readVariable counterIdent
       let (condition, modifierFun) = case forType of
                                        ForUp -> ((counterValue <= lastValue), (\(VInt x) -> (VInt (x+1))))
                                        ForDown -> ((counterValue >= lastValue), (\(VInt x) -> (VInt (x-1))))
       if condition
       then do doAndDropState (execStmt stmt)
               modifyVariable counterIdent modifierFun
               doFor counterIdent forType (VInt lastValue) stmt
       else return ()
readVariable :: Ident -> Execution VarValue
readVariable ident =
 do memstate <- get
    liftIO $ readIORef $ forceFromJust (Map.lookup ident memstate) ("variable not found: " ++ (printTree ident))
modifyVariable :: Ident -> (VarValue -> VarValue) -> Execution ()
modifyVariable ident fun =
 do memstate <- get
    ref <- return $ forceFromJust (Map.lookup ident memstate) ("variable not found: " ++ (printTree ident))
    liftIO $ modifyIORef ref fun
    return ()
