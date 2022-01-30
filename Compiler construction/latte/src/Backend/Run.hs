module  Backend.Run where

import           Backend.Environment
import           Backend.IRGeneration

import           Backend.IR

import           Control.Monad.Reader
import           Control.Monad.State

import qualified Latte.Abs            as AST

compile :: AST.Program -> IO Module
compile tree = evalStateT (runReaderT (emitProgram tree) initEnv) initState
