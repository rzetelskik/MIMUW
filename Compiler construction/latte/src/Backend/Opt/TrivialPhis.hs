{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Backend.Opt.TrivialPhis (opt) where

import           Backend.IR
import           Backend.IRUtils
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List            (nub)
import qualified Data.Map             as Map
import qualified Data.Set             as Set

type TPhisMonad = StateT TPhisState IO

newtype TPhisState = TPhisState { _sVals :: Map.Map Arg Arg }

makeLenses ''TPhisState

initState :: TPhisState
initState = TPhisState { _sVals = Map.empty }

opt :: Module -> IO Module
opt = mapMOf (& mFuncs) (mapM runTPhisFuncDef)
    where
        runTPhisFuncDef :: FuncDef -> IO FuncDef
        runTPhisFuncDef f = evalStateT (tPhisFuncDef f) initState

tPhisFuncDef :: FuncDef -> TPhisMonad FuncDef
tPhisFuncDef = mapMOf (& fBlocks) (mapM tPhisBlock) >=> mapMOf (& fBlocks) (mapM (mapMOf (& bRinstrs) (mapM (mapInstrArgs readArg))))

tPhisBlock :: Block -> TPhisMonad Block
tPhisBlock block = do
    is <- foldM (tPhisInstr (block ^. bLabel)) [] (reverse $ block ^. bRinstrs)
    return $ block & bRinstrs .~ is

tPhisInstr :: Label -> [Instr] -> Instr -> TPhisMonad [Instr]
tPhisInstr l acc i = case i of
    (Instr (Just lhs) (IPhi t args)) -> do
        case nub . filter (/= lhs) . map fst $ args of
            [] -> pure acc
            [unique] -> do
                unique' <- readArg unique
                writeArg lhs unique'
                pure acc
            _ -> pure $ cons i acc
    _ -> pure $ cons i acc

writeArg :: Arg -> Arg -> TPhisMonad ()
writeArg dest val = modifying sVals (at dest ?~ val)

readArg :: Arg  -> TPhisMonad Arg
readArg arg = use (sVals . at arg) >>= \case
    Just v  -> pure v
    Nothing -> pure arg
