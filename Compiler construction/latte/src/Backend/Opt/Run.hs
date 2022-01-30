module Backend.Opt.Run (opt) where

import           Backend.IR

import           Control.Monad

import qualified Backend.Opt.CSE         as CSE
import qualified Backend.Opt.Mem2Reg     as Mem2Reg
import qualified Backend.Opt.TrivialPhis as TrivialPhis

opt :: Module -> IO Module
opt = Mem2Reg.opt >=> fix 50
    where
        fix 0 m = pure m
        fix n m = do
            m' <- TrivialPhis.opt >=> CSE.opt $ m
            if m == m' then pure m' else fix (n - 1) m'
