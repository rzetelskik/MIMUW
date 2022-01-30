{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Backend.LLVM (toLLVM) where

import qualified Data.Text.Lazy             as TL hiding (singleton)
import qualified Data.Text.Lazy.Builder     as TL

import           Control.Lens               (TraversableWithIndex (itraverse),
                                             (^.))

import           Data.Foldable              (foldMap)
import           Data.Text.Lazy.Builder.Int

import           Backend.IR

import qualified Data.Text.Internal.Builder as TL
import qualified Data.Text.Lazy.Builder.Int as TL

toLLVM :: Module -> TL.Text
toLLVM = TL.toLazyText . trans

class Translate a where
    trans :: a -> TL.Builder

    intersperse :: TL.Builder -> [a] -> TL.Builder
    intersperse sep = \case
        []     -> ""
        (x:xs) -> trans x <> prependToAll sep xs

    prependToAll :: TL.Builder -> [a] -> TL.Builder
    prependToAll sep = \case
        []     -> ""
        (x:xs) -> sep <> trans x <> prependToAll sep xs

instance Translate Module where
    trans m =
        foldMap (\d -> trans d <> "\n") (m ^. mDecls) <>
        "\n" <>
        foldMap (\c -> trans c <> "\n") (m ^. mConsts) <>
        "\n" <>
        foldMap (\f -> trans f <> "\n") (m ^. mFuncs)

instance Translate Constant where
    trans c = trans (c ^. cId) <> " = private constant [" <> (TL.decimal . length $ c ^. cVal) <> " x i8] c\"" <> TL.fromString (c ^. cVal) <> "\""

instance Translate Signature where
    trans s = "declare dso_local " <> trans (s ^. sRetType) <> " @" <> trans (s ^. sIdent) <> "(" <>  intersperse ", " (s ^. sArgTypes) <> ")"

instance Translate FuncDef where
    trans fd = "define dso_local " <> trans (fd ^. fRetType) <> " @" <> trans (fd ^. fName) <> "(" <> intersperse ", " (fd ^. fArgs) <> ") {\n" <> foldMap trans (fd ^. fBlocks) <> "}\n"

instance Translate Block where
    trans b = trans (b ^. bLabel) <> ":\n" <> foldMap trans (reverse $ b ^. bRinstrs)

instance Translate Label where
    trans l = "L" <> TL.decimal (unLabel l)

instance Translate Type where
    trans = \case
        TInt       -> "i32"
        TBool      -> "i1"
        TVoid      -> "void"
        TChar      -> "i8"
        TPtr t     -> trans t <> "*"
        TArray s t -> "[" <> decimal s <> " x " <> trans t <> "]"

instance Translate Ident where
    trans = TL.fromLazyText . unIdent

instance Translate Instr where
    trans = \case
        i -> "\t" <> trans' i <> "\n"
        where
            trans' :: Instr -> TL.Builder
            trans' = \case
                (Instr Nothing i)      -> trans i
                (Instr (Just lhs) rhs) -> trans lhs <> " = " <> trans rhs

instance Translate Instr' where
    trans = \case
        (IRet t arg) -> "ret " <> trans t <> " " <> trans arg
        IRetV -> "ret void"
        (IAlloca t) -> "alloca " <> trans t
        (IStore tval val treg reg) -> "store " <> trans tval <> " " <> trans val <> ", " <> trans treg <> " " <> trans reg
        (ILoad tRhs rhs tLhs) -> "load " <> trans tRhs <> ", " <> trans tLhs <> " " <> trans rhs
        (ICall t ident ps) -> "call " <> trans t <> " @" <> trans ident <> "(" <> intersperse ", " ps <> ")"
        (IBitcast tRhs rhs tLhs) -> "bitcast " <> trans tRhs <> " " <> trans rhs <> " to " <> trans tLhs
        (IBinOp t arg1 arg2 op) -> trans op <> " " <> trans t <> " " <> trans arg1 <> ", " <> trans arg2
        (ICmpOp t arg1 arg2 cmpOp) -> "icmp " <> trans cmpOp <> " " <> trans t <> " " <> trans arg1 <> ", " <> trans arg2
        (IXor t arg1 arg2) -> "xor " <> trans t <> " " <> trans arg1 <> ", " <> trans arg2
        (IBranchCond arg lTrue lFalse ) -> "br i1 " <> trans arg <> ", label %" <> trans lTrue <> ", label %" <> trans lFalse
        (IBranch label) -> "br label %" <> trans label
        (IPhi t args) -> "phi " <> trans t <> " " <> intersperse ", " args

instance Translate Op where
    trans = \case
        Add -> "add"
        Sub -> "sub"
        Mul -> "mul"
        Div -> "sdiv"
        Mod -> "srem"

instance Translate CmpOp where
    trans = \case
        LTH -> "slt"
        LE  -> "sle"
        GTH -> "sgt"
        GE  -> "sge"
        NE  -> "ne"
        EQU -> "eq"

instance Translate (Type,Arg) where
    trans (t, arg) = trans t <> " " <> trans arg

instance Translate (Arg, Label) where
    trans (arg, label) = "[ " <> trans arg <> ", %" <> trans label <> " ]"

instance Translate Arg where
    trans = \case
        (AReg i)          -> "%t" <> decimal i
        (ALabeledReg i l) -> "%t" <> decimal i <> "." <> TL.decimal (unLabel l)
        (AImmediate i)    -> decimal i
        (AConst id)       -> trans id

instance Translate ConstId where
    trans id = "@s" <> decimal (unConstId id)
