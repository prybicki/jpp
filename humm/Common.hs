module Common where

import AbsLatte


argIdent :: Arg -> Ident
argIdent (Arg _ ident) = ident

argType :: Arg -> Type
argType (Arg declType _) = declType

funIdent :: TopDef -> Ident
funIdent (FnDef _ ident _ _) = ident

funType :: TopDef -> Type
funType (FnDef declType _ args _) = Fun declType (map argType args)
