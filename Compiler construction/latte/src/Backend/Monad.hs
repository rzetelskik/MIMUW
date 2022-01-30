module Backend.Monad where

import           Backend.Environment

import           Control.Monad.Reader
import           Control.Monad.State

type IRGMonad = ReaderT IRGEnv (StateT IRGState IO)
