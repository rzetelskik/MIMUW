module Frontend.Monad where

import           Frontend.Environment
import           Frontend.Exception

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader

type SemanticAnalysisMonad = ReaderT Env (ExceptT TracedSemanticAnalysisException Identity)
