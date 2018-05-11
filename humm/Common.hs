module Common where

import AbsLatte
import Data.Map as Map hiding(map, foldl)
import Data.List

type TEnv = Map Ident Type
type ErrorMsg = String

emptyTEnv :: TEnv
emptyTEnv = Map.empty

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
builtins = [ (Builtin Void (Ident "print") [(Arg Str (Ident "text"))]) ]
