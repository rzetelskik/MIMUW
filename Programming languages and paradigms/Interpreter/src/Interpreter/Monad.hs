module Interpreter.Monad where

import           Interpreter.Environment
import           Interpreter.Exception

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

type InterpreterMonad = ReaderT Env (ExceptT TracedRuntimeException (StateT Store IO))
