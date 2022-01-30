{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Backend.Opt.CSE (opt) where

import           Backend.IR
import           Backend.IRUtils
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Foldable        as Map
import qualified Data.Map             as Map
import qualified Data.Set             as Set

type CSEMonad = StateT CSEState IO

data CSEState = CSEState { _sDominators :: Map.Map Label (Set.Set Label), _sVals :: Map.Map Arg Arg, _sStmts :: Map.Map Instr' (Label, Arg) }

makeLenses ''CSEState

initState :: CSEState
initState = CSEState { _sDominators = Map.empty, _sVals = Map.empty, _sStmts = Map.empty }

opt :: Module -> IO Module
opt = mapMOf (& mFuncs) (mapM runCSEFuncDef)
    where
        runCSEFuncDef :: FuncDef -> IO FuncDef
        runCSEFuncDef fd = evalStateT (cseFuncDef fd) initState

cseFuncDef :: FuncDef -> CSEMonad FuncDef
cseFuncDef fd = do
    dominators <- getDominators (Map.elems (fd ^. fBlocks))
    assign sDominators dominators

    mapMOf (& fBlocks) (mapM cseBlock) fd

cseBlock :: Block -> CSEMonad Block
cseBlock block = do
    let l = block ^. bLabel
    is <- foldM (\acc i -> mapInstrArgs readArg i >>= cseInstr l acc) [] (reverse $ block ^. bRinstrs)
    return $ block & bRinstrs .~ is

cseInstr :: Label -> [Instr] -> Instr -> CSEMonad [Instr]
cseInstr l acc i@(Instr (Just lhs) rhs) = case rhs of
    ICall {} -> pure $ cons i acc
    _ -> do
        use (sStmts . at rhs) >>= \case
            Nothing -> writeInstr
            Just (source, reg) -> do
                ds <- use (sDominators . ix l)
                if Set.member source ds then writeVal reg else writeInstr
    where
        writeInstr = do
            modifying sStmts (at rhs ?~ (l, lhs))
            pure $ cons i acc

        writeVal reg = do
            modifying sVals (at lhs ?~ reg)
            pure acc

cseInstr _ acc i = pure $ cons i acc

readArg :: Arg -> CSEMonad Arg
readArg arg = use (sVals . at arg) >>= \case
    Just v  -> pure v
    Nothing -> pure arg
