{-# LANGUAGE TemplateHaskell #-}

module Typechecker.Environment where

import           Grammar.Abs

import           Control.Lens
import qualified Data.Map     as Map

type Scope = Integer

data TEnv = TEnv { _types :: Map.Map Ident (Type, Scope), _scope :: Scope, _retType :: Maybe Type, _ret :: Bool }

initTEnv :: TEnv
initTEnv = TEnv { _types = Map.empty , _scope = 0, _retType = Nothing, _ret = False }

makeLenses ''TEnv
