module Interpreter.Run where

import           Grammar.Abs

import           Interpreter.Environment
import           Interpreter.EnvironmentUtils
import           Interpreter.Eval
import           Interpreter.Exception
import           Interpreter.Monad

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

runInterpreterMonad :: Program -> IO (Either TracedRuntimeException (), Store)
runInterpreterMonad program = runStateT (runExceptT (runReaderT (evalProgram program) initEnv)) initStore
