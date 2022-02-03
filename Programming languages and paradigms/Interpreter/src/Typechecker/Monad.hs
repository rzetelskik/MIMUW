module Typechecker.Monad where

import           Typechecker.Environment
import           Typechecker.Exception

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader

type TypecheckerMonad = ReaderT TEnv (ExceptT TracedStaticTypingException Identity)
