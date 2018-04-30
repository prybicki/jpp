import Data.IORef as IORef
import Data.Map as Map


-- data VEnv = VEnv { 1x :: IORef (Map.Map String Int) }

data VariableValue
  = VInt Int
  | VBoolean Bool
  | VString String

-- type VEnv = Map String (IORef VariableValue)

-- assign :: String -> VariableValue -> VEnv -> IO (VEnv)
-- assign name value venv = let var = Map.lookup name venv
--                          in case var of
--                            Nothing -> return venv
--                            do
--                             writeIORef var value
--                             return venv

type VEnv = Map String VariableValue


-- makeCounter :: Int -> IO Counter
-- makeCounter i = do iref <- newIORef i
--                    return (Counter iref)
--
-- incCounter :: Int -> Counter -> IO ()
-- incCounter i (Counter c) = do modifyIORef c (+ i)
--
--
-- showCounter :: Counter -> IO ()
-- showCounter (Counter c) = do c' <- readIORef c
--                              print(c')

main :: IO ()
main = let venv = Map.empty
       in do
          v <- assign "kotek" (VInt 42) venv
          putStrLn "Hello"
