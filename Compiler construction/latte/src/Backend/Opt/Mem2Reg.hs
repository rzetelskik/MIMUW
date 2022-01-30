{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Backend.Opt.Mem2Reg (opt) where

import           Backend.IR
import           Backend.IRUtils
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import qualified Data.Bifunctor      as Bf
import           Data.Key            (TraversableWithKey (mapWithKeyM))
import           Data.List           (nub)
import qualified Data.Map            as Map
import qualified Data.Set            as Set


data M2RState = M2RState { _sPredecessors :: Map.Map Label [Label], _sVals :: Map.Map Label (Map.Map Arg Arg), _sIncompletePhis :: Map.Map Label (Map.Map Arg Instr) }

type M2RMonad = StateT M2RState IO

makeLenses ''M2RState

initState :: M2RState
initState = M2RState { _sPredecessors = Map.empty, _sVals = Map.empty, _sIncompletePhis = Map.empty }

opt :: Module -> IO Module
opt = mapMOf (& mFuncs) (mapM runM2RFuncDef)
    where
        runM2RFuncDef :: FuncDef -> IO FuncDef
        runM2RFuncDef f = evalStateT (m2rFuncDef f) initState

m2rFuncDef :: FuncDef -> M2RMonad FuncDef
m2rFuncDef fd = do
    predecessors <- getPredecessors (Map.elems (fd ^. fBlocks))
    assign sPredecessors predecessors
    mapMOf_ (& fBlocks) (mapM_ initBlockState) fd
    mapMOf (& fBlocks) (mapM m2rBlock) fd >>= mapMOf (& fBlocks) (mapM appendPhis)
    where
        appendPhis :: Block -> M2RMonad Block
        appendPhis block = do
            let l  =  block ^. bLabel
            phis <- use (sIncompletePhis . ix l) >>= mapWithKeyM (appendOperands l)
            pure $ block & bRinstrs %~ (reverse . (\is -> Map.elems phis ++ reverse is))

initBlockState :: Block -> M2RMonad ()
initBlockState block = do
    let l = block ^. bLabel
    modifying sVals (at l ?~ Map.empty)
    modifying sIncompletePhis (at l ?~ Map.empty)

-- Reversed ordering (top-down)
m2rBlock :: Block -> M2RMonad Block
m2rBlock block = do
    is <- foldM (m2rInstr (block ^. bLabel)) [] (reverse $ block ^. bRinstrs)
    return $ block & bRinstrs .~ is

m2rInstr :: Label -> [Instr] -> Instr  -> M2RMonad [Instr]
m2rInstr l acc = \case
    (Instr _ (IAlloca _)) -> pure acc
    (Instr _ (IStore _ arg _ reg)) -> do
        val <- case arg of
            (AReg _) -> do
                m <- use (sVals . ix l)
                case m ^. at arg of
                    Nothing -> pure arg
                    Just v  -> pure v
            _ -> pure arg
        writeArg reg l val
        pure acc
    (Instr (Just lhs) (ILoad t rhs _)) -> do
        val <- readArg rhs t l
        writeArg lhs l val
        pure acc
    (Instr lhs (IPhi t args)) -> do
        args' <- mapM (\(arg, l') -> replaceArg l' arg <&> (,l')) args
        pure $ cons (Instr lhs (IPhi t args')) acc
    i -> mapInstrArgs (replaceArg l) i <&> flip cons acc

writeArg :: Arg -> Label -> Arg -> M2RMonad ()
writeArg dest l val = modifying sVals (at l . _Just . at dest ?~ val)

readArg :: Arg -> Type -> Label -> M2RMonad Arg
readArg arg t l = do
    m <- use (sVals . ix l)
    case m ^. at arg of
        Just v  -> pure v
        Nothing -> readArgRec arg t l

readArgRec :: Arg -> Type -> Label -> M2RMonad Arg
readArgRec arg t l = do
    val <- use (sPredecessors . ix l) >>= \case
        [pred] -> readArg arg t pred
        _ -> do
            let labeledArg = labelVar arg l
            writeArg arg l labeledArg
            addIncompletePhi arg l $ newPhi t labeledArg
    writeArg arg l val
    pure val

labelVar :: Arg -> Label -> Arg
labelVar (AReg v) = ALabeledReg v

unlabelVar :: Arg -> Arg
unlabelVar (ALabeledReg i l) = AReg i

newPhi :: Type -> Arg -> Instr
newPhi t lhs = Instr (Just lhs) (IPhi t [])

addIncompletePhi :: Arg -> Label -> Instr -> M2RMonad Arg
addIncompletePhi arg l i@(Instr (Just lhs) (IPhi t _)) = do
    preds <- use (sPredecessors . ix l) <&> reverse
    -- Call recursive reads just to generate phis. Operands will be appended after all blocks have been processed.
    mapM_ (\l -> readArg arg t l <&> (,l)) preds
    modifying sIncompletePhis $ at l ._Just . at arg ?~ i
    pure lhs

appendOperands :: Label -> Arg  -> Instr -> M2RMonad Instr
appendOperands l arg (Instr (Just lhs) (IPhi t _)) = do
    preds <- use (sPredecessors . ix l) <&> reverse
    ps <- mapM (\l -> readArg arg t l <&> (,l)) preds
    return $ Instr (Just lhs) (IPhi t ps)

replaceArg :: Label -> Arg -> M2RMonad Arg
replaceArg l arg = do
        m <- use (sVals . ix l)
        case m ^. at arg of
            Nothing -> pure arg
            Just v  -> pure v
