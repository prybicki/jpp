{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParGrammar where
import AbsGrammar
import LexGrammar
import ErrM
import qualified Data.Array as Happy_Data_Array
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: (Ident) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> (Ident)
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: (Integer) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> (Integer)
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: (String) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> (String)
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: (Program) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> (Program)
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: (TopDef) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> (TopDef)
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: ([TopDef]) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> ([TopDef])
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: (Arg) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> (Arg)
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: ([Arg]) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> ([Arg])
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: (Block) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> (Block)
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: ([Stmt]) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> ([Stmt])
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: (Stmt) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> (Stmt)
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: (Item) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> (Item)
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: ([Item]) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> ([Item])
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (ForT) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (ForT)
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: (Type) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> (Type)
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: ([Type]) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> ([Type])
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (Expr) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (Expr)
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: (Expr) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> (Expr)
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: (Expr) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> (Expr)
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: (Expr) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> (Expr)
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: (Expr) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> (Expr)
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: (Expr) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> (Expr)
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: (Expr) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> (Expr)
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: ([Expr]) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> ([Expr])
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: (AddOp) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> (AddOp)
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: (MulOp) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> (MulOp)
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: (RelOp) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> (RelOp)
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x64\x01\xbc\x00\x00\x00\xb6\x00\x64\x01\x00\x00\xb7\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd6\x00\x00\x00\x64\x01\xc1\x00\xc4\x00\xb5\x00\x00\x00\xa5\x00\x64\x01\x00\x00\x00\x00\x00\x00\x01\x00\x53\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa4\x00\x00\x00\x00\x00\x04\x00\x05\x00\x68\x01\xa2\x00\xb3\x00\x12\x00\x50\x00\x12\x00\x00\x00\x00\x00\xb1\x00\xae\x00\x4b\x00\x00\x00\xad\x00\x00\x00\x00\x00\x00\x00\x50\x00\xab\x00\xa0\x00\x00\x00\x50\x00\x82\x00\x00\x00\x96\x00\x00\x00\x00\x00\x50\x00\x50\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x8a\x00\x7c\x00\x8b\x00\x50\x00\x89\x00\x79\x00\x50\x00\x66\x00\x00\x00\x00\x00\x69\x00\x68\x00\x00\x00\x44\x00\x50\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x00\x00\x44\x00\x61\x00\x00\x00\x5b\x00\x28\x00\x28\x00\x4f\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x50\x00\x47\x00\x00\x00\x28\x00\x1c\x00\x50\x00\x00\x00\x00\x00\x00\x00\x45\x00\x28\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xbf\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x5d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd2\x00\x00\x00\x00\x00\x57\x00\x00\x00\x3e\x00\xa6\x00\x00\x00\x00\x00\x33\x00\xec\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9f\x00\x00\x00\x00\x00\x1b\x00\x23\x00\x0a\x00\x00\x00\x00\x00\x52\x00\x53\x01\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x48\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3c\x01\x00\x00\x00\x00\x00\x00\x31\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x25\x01\x83\x00\x00\x00\x5f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x2e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x78\x00\x00\x00\x00\x00\x1a\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5a\x00\x0e\x01\x00\x00\x06\x00\x00\x00\xfa\xff\x00\x00\x00\x00\x0e\x00\x00\x00\x00\x00\x00\x00\xd5\x00\xbe\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6b\x00\x00\x00\x00\x00\x03\x01\x00\x00\x00\x00\xa7\x00\xf4\xff\xf7\x00\x00\x00\x00\x00\x00\x00\x00\x00\x90\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\xfe\xff\x00\x00\xf9\xff\xfb\xff\x00\x00\xdb\xff\xdd\xff\xdc\xff\xda\xff\x00\x00\xf8\xff\xf6\xff\xf5\xff\x00\x00\x00\x00\xf7\xff\x00\x00\xf6\xff\xf4\xff\xfa\xff\xf2\xff\x00\x00\xd6\xff\xd5\xff\xd1\xff\xef\xff\xf1\xff\x00\x00\xcd\xff\xcb\xff\xc9\xff\xc7\xff\xc5\xff\xc3\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf0\xff\xd3\xff\x00\x00\x00\x00\x00\x00\xd4\xff\x00\x00\xf3\xff\xfd\xff\xfc\xff\x00\x00\xd6\xff\x00\x00\xe9\xff\x00\x00\x00\x00\xcf\xff\x00\x00\xce\xff\xe4\xff\x00\x00\x00\x00\xb5\xff\x00\x00\xba\xff\xb9\xff\xb6\xff\xb8\xff\xb7\xff\x00\x00\xbf\xff\xbe\xff\x00\x00\xbb\xff\xbd\xff\xbc\xff\xe3\xff\xe1\xff\x00\x00\xc2\xff\x00\x00\x00\x00\x00\x00\x00\x00\xeb\xff\xec\xff\xc1\xff\x00\x00\xee\xff\x00\x00\x00\x00\xcc\xff\xca\xff\xc6\xff\xc8\xff\xc4\xff\xd0\xff\x00\x00\x00\x00\xea\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe2\xff\xe0\xff\xd2\xff\xc2\xff\xed\xff\xc0\xff\x00\x00\xe8\xff\xe6\xff\x00\x00\x00\x00\x00\x00\xde\xff\xdf\xff\xe7\xff\x00\x00\x00\x00\xe5\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x0d\x00\x01\x00\x00\x00\x01\x00\x02\x00\x05\x00\x03\x00\x00\x00\x01\x00\x02\x00\x07\x00\x0b\x00\x08\x00\x00\x00\x0e\x00\x0b\x00\x0d\x00\x18\x00\x10\x00\x11\x00\x12\x00\x15\x00\x05\x00\x10\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x19\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x1a\x00\x24\x00\x25\x00\x26\x00\x27\x00\x01\x00\x18\x00\x04\x00\x05\x00\x05\x00\x00\x00\x01\x00\x02\x00\x1f\x00\x16\x00\x0b\x00\x19\x00\x0e\x00\x0e\x00\x25\x00\x26\x00\x27\x00\x1e\x00\x18\x00\x09\x00\x15\x00\x10\x00\x11\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x08\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x06\x00\x01\x00\x25\x00\x26\x00\x27\x00\x05\x00\x01\x00\x00\x00\x01\x00\x02\x00\x05\x00\x0b\x00\x00\x00\x05\x00\x0e\x00\x00\x00\x0b\x00\x09\x00\x00\x00\x17\x00\x0c\x00\x11\x00\x06\x00\x10\x00\x18\x00\x11\x00\x0b\x00\x0c\x00\x06\x00\x18\x00\x25\x00\x1f\x00\x00\x00\x01\x00\x02\x00\x06\x00\x1f\x00\x25\x00\x26\x00\x27\x00\x0a\x00\x0e\x00\x25\x00\x26\x00\x27\x00\x00\x00\x01\x00\x02\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x01\x00\x02\x00\x0a\x00\x0e\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x01\x00\x02\x00\x10\x00\x11\x00\x12\x00\x13\x00\x0e\x00\x08\x00\x0e\x00\x0a\x00\x11\x00\x06\x00\x1b\x00\x0e\x00\x00\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x0b\x00\x0c\x00\x06\x00\x07\x00\x0e\x00\x08\x00\x05\x00\x0a\x00\x05\x00\x05\x00\x0e\x00\x0e\x00\x05\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x0e\x00\x03\x00\x04\x00\x05\x00\x23\x00\x08\x00\x22\x00\x0a\x00\x25\x00\x06\x00\x0a\x00\x0e\x00\x0e\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x06\x00\x07\x00\x25\x00\x05\x00\x25\x00\x08\x00\x28\x00\x0a\x00\x0e\x00\x25\x00\xff\xff\x0e\x00\xff\xff\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x08\x00\xff\xff\x0a\x00\x00\x00\x01\x00\x02\x00\x0e\x00\xff\xff\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x02\x00\xff\xff\x04\x00\xff\xff\xff\xff\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\xff\xff\xff\xff\x0f\x00\x10\x00\x15\x00\x12\x00\x13\x00\x14\x00\xff\xff\xff\xff\x1b\x00\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x73\x00\x26\x00\x33\x00\x19\x00\x1a\x00\x27\x00\x4a\x00\x33\x00\x19\x00\x1a\x00\x4b\x00\x28\x00\x47\x00\x67\x00\x29\x00\x48\x00\x4c\x00\x45\x00\x1e\x00\x1f\x00\x5c\x00\x08\x00\x27\x00\x38\x00\x2a\x00\x2b\x00\x2c\x00\x09\x00\x2d\x00\x0a\x00\x48\x00\x2e\x00\x0b\x00\x2f\x00\x17\x00\x3d\x00\x30\x00\x03\x00\x31\x00\x32\x00\x26\x00\x2a\x00\x04\x00\x0c\x00\x27\x00\x33\x00\x19\x00\x1a\x00\x2e\x00\x75\x00\x28\x00\x48\x00\x06\x00\x29\x00\x03\x00\x31\x00\x32\x00\x76\x00\x45\x00\x17\x00\x08\x00\x1e\x00\x5b\x00\x2a\x00\x2b\x00\x2c\x00\x09\x00\x2d\x00\x0a\x00\x15\x00\x2e\x00\x0b\x00\x2f\x00\x17\x00\x79\x00\x26\x00\x03\x00\x31\x00\x32\x00\x27\x00\x26\x00\x33\x00\x19\x00\x1a\x00\x27\x00\x28\x00\x11\x00\x50\x00\x36\x00\x4c\x00\x28\x00\x51\x00\x0b\x00\x72\x00\x52\x00\x6f\x00\x66\x00\x3a\x00\x2a\x00\x53\x00\x4d\x00\x69\x00\x67\x00\x2a\x00\x03\x00\x2e\x00\x33\x00\x19\x00\x1a\x00\x6b\x00\x2e\x00\x03\x00\x31\x00\x32\x00\x6c\x00\x6d\x00\x03\x00\x31\x00\x32\x00\x33\x00\x19\x00\x1a\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x56\x00\x6d\x00\x33\x00\x19\x00\x1a\x00\x5a\x00\x55\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x56\x00\x57\x00\x18\x00\x19\x00\x1a\x00\x1e\x00\x1f\x00\x20\x00\x5e\x00\x56\x00\x1b\x00\x59\x00\x79\x00\x5b\x00\x61\x00\x62\x00\x1d\x00\x4c\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x18\x00\x19\x00\x1a\x00\x4d\x00\x4e\x00\x0e\x00\x14\x00\x64\x00\x1b\x00\x50\x00\x76\x00\x33\x00\x37\x00\x10\x00\x1d\x00\x38\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x18\x00\x19\x00\x1a\x00\x3c\x00\x03\x00\x04\x00\x05\x00\x3d\x00\x1b\x00\x17\x00\x6f\x00\x03\x00\x13\x00\x14\x00\x1d\x00\x06\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x18\x00\x19\x00\x1a\x00\x0e\x00\x0f\x00\x03\x00\x0e\x00\x03\x00\x1b\x00\xff\xff\x70\x00\x10\x00\x03\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x33\x00\x19\x00\x1a\x00\x1d\x00\x00\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x33\x00\x19\x00\x1a\x00\x00\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x77\x00\x33\x00\x19\x00\x1a\x00\x00\x00\x00\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x72\x00\x33\x00\x19\x00\x1a\x00\x00\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x68\x00\x33\x00\x19\x00\x1a\x00\x00\x00\x00\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x53\x00\x33\x00\x19\x00\x1a\x00\x00\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x5f\x00\x33\x00\x19\x00\x1a\x00\x00\x00\x00\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x62\x00\x33\x00\x19\x00\x1a\x00\x00\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x64\x00\x33\x00\x19\x00\x1a\x00\x00\x00\x00\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x34\x00\x33\x00\x19\x00\x1a\x00\x00\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x39\x00\x3f\x00\x00\x00\x40\x00\x00\x00\x00\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x5d\x00\x00\x00\x00\x00\x41\x00\x42\x00\x08\x00\x43\x00\x44\x00\x45\x00\x00\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 74) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74)
	]

happy_n_terms = 41 :: Int
happy_n_nonterms = 27 :: Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TV happy_var_1)) -> 
	happyIn4
		 (Ident happy_var_1
	)}

happyReduce_2 = happySpecReduce_1  1# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TI happy_var_1)) -> 
	happyIn5
		 ((read ( happy_var_1)) :: Integer
	)}

happyReduce_3 = happySpecReduce_1  2# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TL happy_var_1)) -> 
	happyIn6
		 (happy_var_1
	)}

happyReduce_4 = happySpecReduce_1  3# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	happyIn7
		 (AbsGrammar.Program happy_var_1
	)}

happyReduce_5 = happyReduce 6# 4# happyReduction_5
happyReduction_5 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { happy_var_2 -> 
	case happyOut11 happy_x_4 of { happy_var_4 -> 
	case happyOut12 happy_x_6 of { happy_var_6 -> 
	happyIn8
		 (AbsGrammar.FnDef happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}}

happyReduce_6 = happySpecReduce_1  5# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn9
		 ((:[]) happy_var_1
	)}

happyReduce_7 = happySpecReduce_2  5# happyReduction_7
happyReduction_7 happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_2 of { happy_var_2 -> 
	happyIn9
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_8 = happySpecReduce_2  6# happyReduction_8
happyReduction_8 happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { happy_var_2 -> 
	happyIn10
		 (AbsGrammar.Arg happy_var_1 happy_var_2
	)}}

happyReduce_9 = happySpecReduce_0  7# happyReduction_9
happyReduction_9  =  happyIn11
		 ([]
	)

happyReduce_10 = happySpecReduce_1  7# happyReduction_10
happyReduction_10 happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	happyIn11
		 ((:[]) happy_var_1
	)}

happyReduce_11 = happySpecReduce_3  7# happyReduction_11
happyReduction_11 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_3 of { happy_var_3 -> 
	happyIn11
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_12 = happySpecReduce_3  8# happyReduction_12
happyReduction_12 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_2 of { happy_var_2 -> 
	happyIn12
		 (AbsGrammar.Block (reverse happy_var_2)
	)}

happyReduce_13 = happySpecReduce_0  9# happyReduction_13
happyReduction_13  =  happyIn13
		 ([]
	)

happyReduce_14 = happySpecReduce_2  9# happyReduction_14
happyReduction_14 happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut14 happy_x_2 of { happy_var_2 -> 
	happyIn13
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_15 = happySpecReduce_1  10# happyReduction_15
happyReduction_15 happy_x_1
	 =  happyIn14
		 (AbsGrammar.Empty
	)

happyReduce_16 = happySpecReduce_1  10# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn14
		 (AbsGrammar.BStmt happy_var_1
	)}

happyReduce_17 = happySpecReduce_3  10# happyReduction_17
happyReduction_17 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_2 of { happy_var_2 -> 
	happyIn14
		 (AbsGrammar.Decl happy_var_1 happy_var_2
	)}}

happyReduce_18 = happyReduce 4# 10# happyReduction_18
happyReduction_18 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut26 happy_x_3 of { happy_var_3 -> 
	happyIn14
		 (AbsGrammar.Ass happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_19 = happySpecReduce_3  10# happyReduction_19
happyReduction_19 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	happyIn14
		 (AbsGrammar.Incr happy_var_1
	)}

happyReduce_20 = happySpecReduce_3  10# happyReduction_20
happyReduction_20 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	happyIn14
		 (AbsGrammar.Decr happy_var_1
	)}

happyReduce_21 = happySpecReduce_3  10# happyReduction_21
happyReduction_21 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_2 of { happy_var_2 -> 
	happyIn14
		 (AbsGrammar.Ret happy_var_2
	)}

happyReduce_22 = happySpecReduce_2  10# happyReduction_22
happyReduction_22 happy_x_2
	happy_x_1
	 =  happyIn14
		 (AbsGrammar.VRet
	)

happyReduce_23 = happyReduce 5# 10# happyReduction_23
happyReduction_23 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut26 happy_x_3 of { happy_var_3 -> 
	case happyOut14 happy_x_5 of { happy_var_5 -> 
	happyIn14
		 (AbsGrammar.Cond happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_24 = happyReduce 7# 10# happyReduction_24
happyReduction_24 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut26 happy_x_3 of { happy_var_3 -> 
	case happyOut14 happy_x_5 of { happy_var_5 -> 
	case happyOut14 happy_x_7 of { happy_var_7 -> 
	happyIn14
		 (AbsGrammar.CondElse happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_25 = happyReduce 5# 10# happyReduction_25
happyReduction_25 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut26 happy_x_3 of { happy_var_3 -> 
	case happyOut14 happy_x_5 of { happy_var_5 -> 
	happyIn14
		 (AbsGrammar.While happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_26 = happyReduce 10# 10# happyReduction_26
happyReduction_26 (happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_4 of { happy_var_4 -> 
	case happyOut26 happy_x_6 of { happy_var_6 -> 
	case happyOut17 happy_x_7 of { happy_var_7 -> 
	case happyOut26 happy_x_8 of { happy_var_8 -> 
	case happyOut14 happy_x_10 of { happy_var_10 -> 
	happyIn14
		 (AbsGrammar.For happy_var_4 happy_var_6 happy_var_7 happy_var_8 happy_var_10
	) `HappyStk` happyRest}}}}}

happyReduce_27 = happySpecReduce_2  10# happyReduction_27
happyReduction_27 happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	happyIn14
		 (AbsGrammar.SExp happy_var_1
	)}

happyReduce_28 = happySpecReduce_1  11# happyReduction_28
happyReduction_28 happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	happyIn15
		 (AbsGrammar.NoInit happy_var_1
	)}

happyReduce_29 = happySpecReduce_3  11# happyReduction_29
happyReduction_29 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut26 happy_x_3 of { happy_var_3 -> 
	happyIn15
		 (AbsGrammar.Init happy_var_1 happy_var_3
	)}}

happyReduce_30 = happySpecReduce_1  12# happyReduction_30
happyReduction_30 happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	happyIn16
		 ((:[]) happy_var_1
	)}

happyReduce_31 = happySpecReduce_3  12# happyReduction_31
happyReduction_31 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn16
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_32 = happySpecReduce_1  13# happyReduction_32
happyReduction_32 happy_x_1
	 =  happyIn17
		 (AbsGrammar.ForUp
	)

happyReduce_33 = happySpecReduce_1  13# happyReduction_33
happyReduction_33 happy_x_1
	 =  happyIn17
		 (AbsGrammar.ForDown
	)

happyReduce_34 = happySpecReduce_1  14# happyReduction_34
happyReduction_34 happy_x_1
	 =  happyIn18
		 (AbsGrammar.Int
	)

happyReduce_35 = happySpecReduce_1  14# happyReduction_35
happyReduction_35 happy_x_1
	 =  happyIn18
		 (AbsGrammar.Str
	)

happyReduce_36 = happySpecReduce_1  14# happyReduction_36
happyReduction_36 happy_x_1
	 =  happyIn18
		 (AbsGrammar.Bool
	)

happyReduce_37 = happySpecReduce_1  14# happyReduction_37
happyReduction_37 happy_x_1
	 =  happyIn18
		 (AbsGrammar.Void
	)

happyReduce_38 = happySpecReduce_0  15# happyReduction_38
happyReduction_38  =  happyIn19
		 ([]
	)

happyReduce_39 = happySpecReduce_1  15# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 ((:[]) happy_var_1
	)}

happyReduce_40 = happySpecReduce_3  15# happyReduction_40
happyReduction_40 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut19 happy_x_3 of { happy_var_3 -> 
	happyIn19
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_41 = happySpecReduce_1  16# happyReduction_41
happyReduction_41 happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	happyIn20
		 (AbsGrammar.EVar happy_var_1
	)}

happyReduce_42 = happySpecReduce_1  16# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	happyIn20
		 (AbsGrammar.ELitInt happy_var_1
	)}

happyReduce_43 = happySpecReduce_1  16# happyReduction_43
happyReduction_43 happy_x_1
	 =  happyIn20
		 (AbsGrammar.ELitTrue
	)

happyReduce_44 = happySpecReduce_1  16# happyReduction_44
happyReduction_44 happy_x_1
	 =  happyIn20
		 (AbsGrammar.ELitFalse
	)

happyReduce_45 = happyReduce 4# 16# happyReduction_45
happyReduction_45 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut27 happy_x_3 of { happy_var_3 -> 
	happyIn20
		 (AbsGrammar.EApp happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_46 = happySpecReduce_1  16# happyReduction_46
happyReduction_46 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn20
		 (AbsGrammar.EString happy_var_1
	)}

happyReduce_47 = happySpecReduce_3  16# happyReduction_47
happyReduction_47 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_2 of { happy_var_2 -> 
	happyIn20
		 (happy_var_2
	)}

happyReduce_48 = happySpecReduce_2  17# happyReduction_48
happyReduction_48 happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_2 of { happy_var_2 -> 
	happyIn21
		 (AbsGrammar.Neg happy_var_2
	)}

happyReduce_49 = happySpecReduce_2  17# happyReduction_49
happyReduction_49 happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_2 of { happy_var_2 -> 
	happyIn21
		 (AbsGrammar.Not happy_var_2
	)}

happyReduce_50 = happySpecReduce_1  17# happyReduction_50
happyReduction_50 happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	happyIn21
		 (happy_var_1
	)}

happyReduce_51 = happySpecReduce_3  18# happyReduction_51
happyReduction_51 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut29 happy_x_2 of { happy_var_2 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	happyIn22
		 (AbsGrammar.EMul happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_52 = happySpecReduce_1  18# happyReduction_52
happyReduction_52 happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 (happy_var_1
	)}

happyReduce_53 = happySpecReduce_3  19# happyReduction_53
happyReduction_53 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_2 of { happy_var_2 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 (AbsGrammar.EAdd happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_54 = happySpecReduce_1  19# happyReduction_54
happyReduction_54 happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	happyIn23
		 (happy_var_1
	)}

happyReduce_55 = happySpecReduce_3  20# happyReduction_55
happyReduction_55 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_2 of { happy_var_2 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 (AbsGrammar.ERel happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_56 = happySpecReduce_1  20# happyReduction_56
happyReduction_56 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn24
		 (happy_var_1
	)}

happyReduce_57 = happySpecReduce_3  21# happyReduction_57
happyReduction_57 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn25
		 (AbsGrammar.EAnd happy_var_1 happy_var_3
	)}}

happyReduce_58 = happySpecReduce_1  21# happyReduction_58
happyReduction_58 happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn25
		 (happy_var_1
	)}

happyReduce_59 = happySpecReduce_3  22# happyReduction_59
happyReduction_59 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	case happyOut26 happy_x_3 of { happy_var_3 -> 
	happyIn26
		 (AbsGrammar.EOr happy_var_1 happy_var_3
	)}}

happyReduce_60 = happySpecReduce_1  22# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn26
		 (happy_var_1
	)}

happyReduce_61 = happySpecReduce_0  23# happyReduction_61
happyReduction_61  =  happyIn27
		 ([]
	)

happyReduce_62 = happySpecReduce_1  23# happyReduction_62
happyReduction_62 happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	happyIn27
		 ((:[]) happy_var_1
	)}

happyReduce_63 = happySpecReduce_3  23# happyReduction_63
happyReduction_63 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut27 happy_x_3 of { happy_var_3 -> 
	happyIn27
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_64 = happySpecReduce_1  24# happyReduction_64
happyReduction_64 happy_x_1
	 =  happyIn28
		 (AbsGrammar.Plus
	)

happyReduce_65 = happySpecReduce_1  24# happyReduction_65
happyReduction_65 happy_x_1
	 =  happyIn28
		 (AbsGrammar.Minus
	)

happyReduce_66 = happySpecReduce_1  25# happyReduction_66
happyReduction_66 happy_x_1
	 =  happyIn29
		 (AbsGrammar.Times
	)

happyReduce_67 = happySpecReduce_1  25# happyReduction_67
happyReduction_67 happy_x_1
	 =  happyIn29
		 (AbsGrammar.Div
	)

happyReduce_68 = happySpecReduce_1  25# happyReduction_68
happyReduction_68 happy_x_1
	 =  happyIn29
		 (AbsGrammar.Mod
	)

happyReduce_69 = happySpecReduce_1  26# happyReduction_69
happyReduction_69 happy_x_1
	 =  happyIn30
		 (AbsGrammar.LTH
	)

happyReduce_70 = happySpecReduce_1  26# happyReduction_70
happyReduction_70 happy_x_1
	 =  happyIn30
		 (AbsGrammar.LE
	)

happyReduce_71 = happySpecReduce_1  26# happyReduction_71
happyReduction_71 happy_x_1
	 =  happyIn30
		 (AbsGrammar.GTH
	)

happyReduce_72 = happySpecReduce_1  26# happyReduction_72
happyReduction_72 happy_x_1
	 =  happyIn30
		 (AbsGrammar.GE
	)

happyReduce_73 = happySpecReduce_1  26# happyReduction_73
happyReduction_73 happy_x_1
	 =  happyIn30
		 (AbsGrammar.EQU
	)

happyReduce_74 = happySpecReduce_1  26# happyReduction_74
happyReduction_74 happy_x_1
	 =  happyIn30
		 (AbsGrammar.NE
	)

happyNewToken action sts stk [] =
	happyDoAction 40# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TS _ 31) -> cont 31#;
	PT _ (TS _ 32) -> cont 32#;
	PT _ (TS _ 33) -> cont 33#;
	PT _ (TS _ 34) -> cont 34#;
	PT _ (TS _ 35) -> cont 35#;
	PT _ (TS _ 36) -> cont 36#;
	PT _ (TV happy_dollar_dollar) -> cont 37#;
	PT _ (TI happy_dollar_dollar) -> cont 38#;
	PT _ (TL happy_dollar_dollar) -> cont 39#;
	_ -> happyError' (tk:tks)
	}

happyError_ 40# tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => [(Token)] -> Err a
happyError' = happyError

pProgram tks = happySomeParser where
  happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut7 x))

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 11 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4










































{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}





-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 46 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList





{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}


          case action of
                0#           -> {- nothing -}
                                     happyFail i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}


                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = indexShortOffAddr happyActOffsets st
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st


indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#





data HappyAddr = HappyA# Happy_GHC_Exts.Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 170 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = indexShortOffAddr happyGotoOffsets st1
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i



          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = indexShortOffAddr happyGotoOffsets st
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
