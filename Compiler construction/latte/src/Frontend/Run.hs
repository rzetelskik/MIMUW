module Frontend.Run where

import           Latte.Abs

import           Frontend.Environment
import           Frontend.Exception
import           Frontend.SemanticAnalysis

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader

runSemanticAnalysisMonad :: Program -> Either TracedSemanticAnalysisException ()
runSemanticAnalysisMonad program = runIdentity (runExceptT (runReaderT (analyseProgram program) initEnv))
