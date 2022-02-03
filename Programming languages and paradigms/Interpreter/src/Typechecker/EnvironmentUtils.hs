{-# LANGUAGE TupleSections #-}

module Typechecker.EnvironmentUtils where

import           Grammar.Abs

import           Common.Exception
import           Typechecker.Environment
import           Typechecker.Exception
import           Typechecker.Monad

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader

import qualified Data.Map                as Map

extractType :: BNFC'Position -> Ident -> TypecheckerMonad Type
extractType pos i = do
  env <- ask

  case env ^. types . at i of
    Just (t, _) -> return t
    _           -> throwPosError pos $ Undefined i

declareIdent :: BNFC'Position -> Ident -> Type -> TypecheckerMonad TEnv
declareIdent pos i t = do
  throwIfRedeclared pos i
  env <- ask

  return $ env & (types . at i ?~ (t, env ^. scope))


throwIfRedeclared :: BNFC'Position -> Ident -> TypecheckerMonad ()
throwIfRedeclared pos i = do
  env <- ask

  case Map.lookup i (env ^. types) of
    Just (_, vScope) | vScope >= (env ^. scope) -> throwPosError pos $ Redeclared i
    _                                           -> return ()

flattenArgs :: [Arg] -> [(Ident,Type)]
flattenArgs = concatMap (\(Ar _ idents t) -> map (, t) idents)
