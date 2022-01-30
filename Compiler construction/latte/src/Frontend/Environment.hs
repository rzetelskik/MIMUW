{-# LANGUAGE TemplateHaskell #-}

module Frontend.Environment where

import           Latte.Abs

import           Frontend.TypeUtils

import           Control.Lens
import qualified Data.Map                   as Map

type Scope = Integer

data Env = Env { _types :: Map.Map Ident (Type, Scope), _scope :: Scope, _retType :: Type, _ret :: Bool }

initFuncs :: [(Ident, (Type,Scope))]
initFuncs = [
    (Ident "printInt", (Fun NoPos (Void NoPos) [Int NoPos], 0)),
    (Ident "printString", (Fun NoPos (Void NoPos) [Str NoPos], 0)),
    (Ident "error", (Fun NoPos (Void NoPos) [], 0)),
    (Ident "readInt", (Fun NoPos (Int NoPos) [], 0)),
    (Ident "readString", (Fun NoPos (Str NoPos) [], 0)),
    (Ident "_concat", (Fun NoPos (Str NoPos) [Str NoPos, Str NoPos], 0)),
    (Ident "_strcmp", (Fun NoPos (Bool NoPos) [Str NoPos, Str NoPos], 0))
    ]

initEnv :: Env
initEnv = Env { _types = Map.fromList initFuncs , _scope = 0, _retType = Void NoPos, _ret = False }

makeLenses ''Env
