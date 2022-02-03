{-# LANGUAGE TemplateHaskell #-}

module Interpreter.Environment where

import           Grammar.Abs

import           Control.Lens
import           Data.Char    (toLower)
import qualified Data.Map     as Map
import           Numeric      (showHex)

newtype Env = Env { _mem :: Map.Map Ident Loc } deriving Show

initEnv :: Env
initEnv = Env { _mem = Map.empty }

type Store = Map.Map Loc MVal

initStore :: Store
initStore = Map.empty

type Loc = Integer

newtype FuncArg = FuncArg Ident deriving Show

data MVal
    = BoolV Bool
    | IntV Integer
    | StringV String
    | PtrV Loc
    | NilV
    | VoidV
    | FuncDefV [FuncArg] Type Block Env

instance Show MVal where
    show (BoolV b)   = map toLower (show b)
    show (IntV i)    = show i
    show (StringV s) = s
    show (PtrV l)    = "0x" ++ showHex l ""
    show NilV        = "<nil>"
    show VoidV       = undefined
    show FuncDefV{}  = undefined

makeLenses ''Env
