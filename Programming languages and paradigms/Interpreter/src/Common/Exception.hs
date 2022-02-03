{-# LANGUAGE FlexibleContexts #-}

module Common.Exception where

import           Grammar.Abs

import           Control.Monad.Except

type TracedException e = (BNFC'Position, e)

throwPosError :: MonadError (TracedException e) m => BNFC'Position -> e -> m a
throwPosError = curry throwError
