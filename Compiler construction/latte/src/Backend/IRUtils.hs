{-# LANGUAGE GADTs         #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
module Backend.IRUtils where

import           Backend.IR

import           Control.Lens
import           Control.Monad

import           Data.Key      (TraversableWithKey (mapWithKeyM))
import qualified Data.Map      as Map
import qualified Data.Set      as Set

returns :: Maybe Instr -> Bool
returns = \case
    Nothing -> False
    Just (Instr _ i) -> case i of
        IRet {} -> True
        IRetV   -> True
        _       -> False

mapInstrArgs :: Monad m => (Arg -> m Arg) -> Instr -> m Instr
mapInstrArgs f (Instr lhs i) = mapInstrArgs' i <&> Instr lhs
    where
        mapInstrArgs' = \case
            (IRet t arg) -> f arg <&> IRet t
            (ICall t ident ps) -> do
                ps' <- mapM (\(t, arg) -> f arg <&> (t,)) ps
                pure $ ICall t ident ps'
            (IBitcast tRhs rhs tLhs) -> do
                rhs' <- f rhs
                pure $ IBitcast tRhs rhs' tLhs
            (IBinOp t arg1 arg2 op) -> do
                arg1' <- f arg1
                arg2' <- f arg2
                pure $ IBinOp t arg1' arg2' op
            (ICmpOp t arg1 arg2 cmpOp) -> do
                arg1' <- f arg1
                arg2' <- f arg2
                pure $ ICmpOp t arg1' arg2' cmpOp
            (IXor t arg1 arg2) -> do
                arg1' <- f arg1
                arg2' <- f arg2
                pure $ IXor t arg1' arg2'
            (IBranchCond arg lTrue lFalse) -> do
                arg' <- f arg
                pure $ IBranchCond arg' lTrue lFalse
            (IPhi t args) -> do
                args' <- mapM (\(arg, l) -> f arg <&> (,l)) args
                pure $ IPhi t args'
            i' -> pure i'

getDominators :: Monad m => [Block] -> m (Map.Map Label (Set.Set Label))
getDominators blocks = do
    preds <- getPredecessors blocks
    let m = foldl (\acc n -> acc & at n ?~ nodes) Map.empty (nodes Set.\\ rootSet) & at root ?~ rootSet
    go nodes preds m
        where
            root = Label 0
            rootSet = Set.singleton root
            nodes = Set.fromList $ map (^. bLabel) blocks
            go nodes preds m = do
                m' <- mapWithKeyM (\n a -> if n == root then pure rootSet else pure $ Set.union (Set.singleton n) (foldl (\acc p -> acc `Set.intersection` (m ^. ix p)) nodes (preds ^. ix n))) m
                if m' == m then pure m' else go nodes preds m'

getPredecessors :: Monad m => [Block] -> m (Map.Map Label [Label])
getPredecessors = foldM (\acc b -> getSuccessors b <&> foldl (add b) acc) Map.empty
    where
        add block acc successor = acc & at successor . non [] %~ cons (block ^. bLabel)

getSuccessors :: Monad m => Block -> m [Label]
getSuccessors block = case head (block ^. bRinstrs) of
    (Instr _ (IBranch l))           -> pure [l]
    (Instr _ (IBranchCond _ l1 l2)) -> pure [l1, l2]
    _                               -> pure []
