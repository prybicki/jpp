{-# LANGUAGE LambdaCase #-}

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

-- Guarantees from static checker:
-- → All identifiers in any scope are unique and has unequivocal type
-- → All returns have correct type (as enclosing function return type)
-- → Expressions have correct typing
-- → There are no topdefs colliding with builtins
-- → All referenced variables exist in scope
-- → All called functions exist in scope

-- TODO:
-- Make static checker great again (use monads), add collision checking
-- Make all needed builtins
-- Refactor code to be more compact
-- Test, test, test

data VarValue
  = VVoid
  | VInt Integer
  | VBoolean Bool
  | VString String
  deriving (Show, Eq, Ord)

type FEnv = Map Ident TopDef
type MemState = Map Ident (IORef VarValue)
data ExceptionType = ReturnException VarValue | ErrorException String
type Execution a = ReaderT FEnv (StateT MemState (ExceptT ExceptionType IO)) a

--------------------------------------------------------------------------------

typeDefaultValue :: Type -> VarValue
typeDefaultValue declType = case declType of
  Int -> VInt 0
  Str -> VString ""
  Bool -> VBoolean False

fromJust :: Maybe a -> String -> a
fromJust Nothing errMsg = error errMsg
fromJust (Just x) errMsg = x


-- addVariables :: [(Ident, Expr)] -> MemState -> MemState
-- addVariables lst memstate = Map.empty

withStateDo state action =
  do oldState <- get
     put state
     ret <- action
     put oldState
     return ret

-- Todo na from list
-- makeStateFromArgsExprs :: [(Ident, Value)] -> MemState
-- makeStateFromArgs lst = aux Map.empty lst where
--   aux memstate [] = memstate
--   aux
--

evalExpr :: Expr -> Execution VarValue
evalExpr expr = case expr of
  EVar ident ->
    do state <- get
       liftIO $ readIORef $ fromJust (Map.lookup ident state) ("variable not found: " ++ (printTree ident))
  ELitInt n -> return (VInt n)
  ELitTrue -> return (VBoolean True)
  ELitFalse -> return (VBoolean False)
  EApp ident exprs ->
    do memstate <- get
       fenv <- ask
       -- let (FnDef declType _ args block) = fromJust (Map.lookup ident fenv) ("topdef not found: " ++ (printTree ident))
       -- values <- mapM evalExpr exprs
       -- ioValues <- mapM (liftIO . newIORef) values
       -- do {
       --    withStateDo (Map.fromList (zip (map argIdent args) ioValues)) (execStmt (BStmt block));
       --    return (typeDefaultValue declType)
       -- }
       -- `catchError` (\case
       --   ReturnException varValue -> return varValue
       --   err -> throwError err)
       -- ---
       let topdef = fromJust (Map.lookup ident fenv) ("topdef not found: " ++ (printTree ident))
       case topdef of
         (Builtin declType _ args) ->
           do values <- mapM evalExpr exprs
              ioValues <- mapM (liftIO . newIORef) values
              do {
                withStateDo (Map.fromList (zip (map argIdent args) ioValues)) (execBuiltin ident);
                error ("Builtin did not returned any value ;(")
             }
             `catchError` (\case
               ReturnException varValue -> return varValue
               err -> throwError err)
         (FnDef declType _ args block) ->
           do values <- mapM evalExpr exprs
              ioValues <- mapM (liftIO . newIORef) values
              liftIO $ putStrLn ("calling " ++ (printTree ident) ++ " " ++ (show values) ++ " from: " ++ (printTree exprs))
              do {
                 withStateDo (Map.fromList (zip (map argIdent args) ioValues)) (execStmt (BStmt block));
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
  EAdd expr1 op expr2 ->
    do (VInt value1) <- evalExpr expr1
       (VInt value2) <- evalExpr expr2
       case op of
         Plus -> return (VInt (value1 + value2))
         Minus -> return (VInt (value1 - value2))
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


execBuiltin :: Ident -> Execution VarValue
execBuiltin (Ident name) = case name of
  "print" -> -- void print(string text)
    do memstate <- get
       (VString text) <- liftIO $ readIORef $ fromJust (Map.lookup (Ident "text") memstate) "builtin argument not found: text"
       liftIO $ putStrLn text
       throwError (ReturnException VVoid)
  _ -> error("Congratulations, you succeded to call non-existing builtin!")


execStmt :: Stmt -> Execution ()
execStmt stmt = case stmt of
  Empty -> return ()
  BStmt (Block stmts) -> mapM_ execStmt stmts
  Decl declType [] -> return ()
  Decl declType (item:t) -> case item of
    NoInit ident -> do ioValue <- liftIO $ newIORef (typeDefaultValue declType)
                       modify (Map.insert ident ioValue)
                       return ()
    Init ident expr -> do exprValue <- evalExpr expr
                          ioValue <- liftIO $ newIORef exprValue
                          modify (Map.insert ident ioValue)
                          return ()
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
       else do execAndDropDeclarations stmt
  CondElse expr ifstmt elsestmt ->
    do (VBoolean condition) <- evalExpr expr
       if condition
       then do execAndDropDeclarations ifstmt
       else do execAndDropDeclarations elsestmt
  While expr stmt ->
    do (VBoolean condition) <- evalExpr expr
       if condition
       then do execAndDropDeclarations stmt
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
       then do execAndDropDeclarations stmt
               modifyVariable counterIdent modifierFun
               doFor counterIdent forType (VInt lastValue) stmt
       else return ()
   -- TODO: wydzielić wszystkie powtarzające się idiomy do funkcji
   readVariable :: Ident -> Execution VarValue
   readVariable ident =
    do memstate <- get
       liftIO $ readIORef $ fromJust (Map.lookup ident memstate) ("variable not found: " ++ (printTree ident))
   modifyVariable :: Ident -> (VarValue -> VarValue) -> Execution ()
   modifyVariable ident fun =
    do memstate <- get
       ref <- return $ fromJust (Map.lookup ident memstate) ("variable not found")
       liftIO $ modifyIORef ref fun
       return ()
   execAndDropDeclarations :: Stmt -> Execution ()
   execAndDropDeclarations stmt =
    do memstate <- get
       execStmt stmt
       put memstate
       return ()

builtins :: [(Ident, TopDef)]
builtins =
 [ ((Ident "print"), (Builtin Void (Ident "print") [(Arg Str (Ident "text"))])) ]

makeFEnv :: [TopDef] -> FEnv
makeFEnv topdefs = Map.fromList (((zip (map funIdent topdefs)) topdefs) ++ builtins)

parseProgram :: String -> Except Error Program
parseProgram str =
  case pProgram (myLexer str) of
    Bad errMsg -> throwError ("Parse error: " ++ errMsg)
    Ok parsedProgram -> return parsedProgram

makeGoodProgram :: String -> Except Error Program
makeGoodProgram str =
  do parsedProgram <- parseProgram str
     -- checkedProgram <- checkProgram' parsedProgram
     return parsedProgram ---------------------------------------------------------TODO ADD Checker and implement builtins there
  where -- TODO Remove when StaticCheck uses Except
    checkProgram' :: Program -> Except Error Program
    checkProgram' program = case checkProgram program of
      Left err -> throwError err
      Right () -> return program


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
    startingEnv = makeFEnv topdefs
    startingMemState = Map.fromList []

main = do
  args <- getArgs
  fileStr <- readFile $ args !! 0
  let maybeProgram = runExcept $ makeGoodProgram fileStr
  case maybeProgram of
    Left err -> putStrLn err
    Right program -> execProgram program

  --   checkProgram program
  --   return program
  -- case program of
  --   Left err ->
  --
  -- case parseResult of
  --  Bad err -> putStrLn ("Failed to parse program: " ++ err)
  --  Ok program -> do goodProgram <- checkProgram program
  --                   return goodProgram



































  -- case (checkProgram program) of
  --   Left err -> putStrLn err
  --   Right () -> case program of
  --     (Program []) -> error("static")
  --     (Program lst) -> let Just (FnDef _ _ _ block) = find (\(FnDef _ ident _ _) -> ident == (Ident "main")) lst
  --                      in do a <- runExceptT (runStateT (runReaderT (execStmt (BStmt block)) (makeFEnv (Program lst))) (Map.fromList []))
  --                         print ("OK")


                                 -- execStmt :: Integer -> Execution VarValue
                                 -- execStmt stmt
                                 --   | stmt < 5 = do iovar <- liftIO $ newIORef (VInt stmt)
                                 --                   modify (Map.insert (Ident "var") iovar)
                                 --                   liftIO $ putStrLn "added var"
                                 --                   liftIO $ putStrLn $ show (foldl (+) 0 [1..10000])
                                 --                   throwError ("meh")


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
