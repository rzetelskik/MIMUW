module Frontend.EnvironmentUtils where

import           Latte.Abs

import           Frontend.Environment
import           Frontend.Exception
import           Frontend.Monad

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader

import qualified Data.Map                     as Map

extractType :: BNFC'Position -> Ident -> SemanticAnalysisMonad Type
extractType pos i = do
  env <- ask

  case env ^. types . at i of
    Just (t, _) -> return t
    _           -> throwPosError pos $ Undefined i

declareIdent :: BNFC'Position -> Ident -> Type -> SemanticAnalysisMonad Env
declareIdent pos i t = do
  throwIfRedeclared pos i
  env <- ask

  return $ env & (types . at i ?~ (t, env ^. scope))


throwIfRedeclared :: BNFC'Position -> Ident -> SemanticAnalysisMonad ()
throwIfRedeclared pos i = do
  env <- ask

  case Map.lookup i (env ^. types) of
    Just (_, vScope) | vScope >= (env ^. scope) -> throwPosError pos $ Redeclared i
    _                                           -> return ()
