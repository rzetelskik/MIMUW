module Typechecker.Run where

import           Grammar.Abs

import           Typechecker.Environment
import           Typechecker.Exception
import           Typechecker.Typecheck

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader

runTypecheckerMonad :: Program -> Either TracedStaticTypingException ()
runTypecheckerMonad program = runIdentity (runExceptT (runReaderT (typecheckProgram program) initTEnv))
