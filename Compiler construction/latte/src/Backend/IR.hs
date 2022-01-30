{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Backend.IR where

import qualified Data.Map       as Map
import qualified Data.Text.Lazy as TL

import           Control.Lens

data Module = Module { _mFuncs :: [FuncDef], _mConsts :: [Constant], _mDecls :: [Signature] } deriving (Eq)

data FuncDef = FuncDef { _fName :: Ident, _fRetType :: Type, _fArgs :: [(Type, Arg)], _fBlocks :: Map.Map Label Block } deriving (Eq)

data Constant = Constant { _cId :: ConstId, _cVal :: String } deriving (Eq)

data Signature = Signature { _sIdent :: Ident, _sRetType :: Type, _sArgTypes :: [Type] } deriving (Eq)

data Block = Block { _bLabel :: Label, _bRinstrs :: [Instr] } deriving (Eq)

newtype Label = Label { unLabel :: Integer } deriving (Eq, Ord)

newtype ConstId = ConstId { unConstId :: Integer } deriving (Eq, Ord)

newtype Ident = Ident { unIdent :: TL.Text } deriving (Eq, Ord)

data Arg
    = AImmediate Integer
    | AReg Integer
    | ALabeledReg Integer Label
    | AConst ConstId
    | AUndefined
    deriving (Eq, Ord)

data Type
    = TInt
    | TBool
    | TChar
    | TArray Integer Type
    | TPtr Type
    | TVoid
    deriving (Eq, Ord)

data Instr = Instr (Maybe Arg) Instr' deriving (Eq)

data Instr'
    = IRet Type Arg
    | IRetV
    | IAlloca Type
    | IStore Type Arg Type Arg
    | ILoad Type Arg Type
    | ICall Type Ident [(Type, Arg)]
    | IBitcast Type Arg Type
    | IBinOp Type Arg Arg Op
    | ICmpOp Type Arg Arg CmpOp
    | IXor Type Arg Arg
    | IBranchCond Arg Label Label
    | IBranch Label
    | IPhi Type [(Arg, Label)]
    deriving (Eq, Ord)

data Op
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    deriving (Eq, Ord)

data CmpOp
    = LTH
    | LE
    | GTH
    | GE
    | EQU
    | NE
    deriving (Eq, Ord)

makeLenses ''Module
makeLenses ''Constant
makeLenses ''FuncDef
makeLenses ''Signature
makeLenses ''Block

