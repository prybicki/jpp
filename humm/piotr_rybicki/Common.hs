module Common where

import AbsLatte
import Data.Map as Map hiding(map, foldl)
import Data.List

import Control.Monad.State
import Control.Monad.Except

type ErrorMsg = String

argIdent :: Arg -> Ident
argIdent (Arg _ ident) = ident

argType :: Arg -> Type
argType (Arg declType _) = declType

argTypes :: [Arg] -> [Type]
argTypes = map argType

funIdent :: TopDef -> Ident
funIdent (FnDef _ ident _ _) = ident
funIdent (Builtin _ ident _) = ident

funType :: TopDef -> Type
funType (FnDef declType _ args _) = Fun declType (map argType args)
funType (Builtin declType _ args) = Fun declType (map argType args)

elementsUnique :: Ord a => [a] -> Bool
elementsUnique lst = length (group (sort lst)) == length lst

builtins :: [TopDef]
builtins = [ (Builtin Void (Ident "print")    [(Arg Str (Ident "text"))]),
             (Builtin Int  (Ident "strToInt") [(Arg Str (Ident "text"))]),
             (Builtin Str  (Ident "intToStr") [(Arg Int (Ident "number"))])]

doAndDropState :: MonadState s m => m a -> m ()
doAndDropState action =
 do state <- get
    action
    put state

tryFromJust :: MonadError e m => Maybe a -> e -> m a
tryFromJust Nothing errMsg = throwError errMsg
tryFromJust (Just x) _ = return x

forceFromJust :: Maybe a -> String -> a
forceFromJust Nothing errMsg = error errMsg
forceFromJust (Just x) errMsg = x
