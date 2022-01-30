{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Backend.IRGeneration where

import qualified Data.Map                 as Map
import qualified Data.Text.Lazy           as TL
import qualified Latte.Abs                as AST


import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State

import           Backend.Environment
import           Backend.EnvironmentUtils
import           Backend.IR
import           Backend.IRUtils
import           Backend.Monad
import           Data.Maybe
import           System.IO                (print, stdout)

libSignatures :: [Signature]
libSignatures = [
    Signature (Ident "printInt") TInt [TInt],
    Signature (Ident "printString") TVoid [TPtr TChar],
    Signature (Ident "error") TVoid [],
    Signature (Ident "readInt") TInt [],
    Signature (Ident "readString") (TPtr TChar) [],
    Signature (Ident "_concat") (TPtr TChar) [TPtr TChar, TPtr TChar],
    Signature (Ident "_strcmp") TBool [TPtr TChar, TPtr TChar]
    ]

emitProgram :: AST.Program -> IRGMonad Module
emitProgram (AST.Program _ topdefs) = do
    mapM_ declareSignature $ libSignatures ++ map getSignature topdefs
    _mFuncs <- mapM emitTopDef topdefs
    _mConsts <- gets (Map.toList . (^. storeConsts)) >>= mapM (\(_cVal, _cId) -> pure Constant { _cId, _cVal})
    return $ Module { _mFuncs, _mConsts , _mDecls = libSignatures}

class HasSignature a where
    getSignature :: a -> Signature

instance HasSignature AST.TopDef where
    getSignature = \case
        (AST.FnDef _ t i args _ ) -> Signature (convert i) (convert t) $ map (\(AST.Arg _ t _) -> convert t) args

emitTopDef :: AST.TopDef -> IRGMonad FuncDef
emitTopDef (AST.FnDef _ t i args block) = do
    refreshState
    lEntry <- getLabel
    useBlock lEntry
    let (argTypes, argIdents) = unzip $ map convert args
    temps <- mapM (const newTemp) args
    env <- ask
    env' <- foldM (\env -> local (const env) . storeArg) env (zip3 temps argTypes argIdents)
    local (const env') (emitBlock block)
    when (convert t == TVoid) verifyVoidReturn
    blocks <- gets (^. blocks) <&> Map.filter isBlockNonEmpty
    return $ FuncDef (convert i) (convert t) (zip argTypes temps) blocks
    where
        storeArg :: (Arg,Type,Ident) -> IRGMonad IRGEnv
        storeArg (temp, t, i) = do
            reg <- newTemp
            emit $ Instr (Just reg) (IAlloca t)
            emit $ Instr Nothing (IStore t temp (TPtr t) reg)
            declareIdent i (t, reg)

        verifyVoidReturn :: IRGMonad ()
        verifyVoidReturn = do
            returns <- use currBlock >>= blockReturns
            unless returns (emit $ Instr Nothing IRetV)

        isBlockNonEmpty :: Block -> Bool
        isBlockNonEmpty = not . Prelude.null . view bRinstrs

emitBlock :: AST.Block -> IRGMonad IRGEnv
emitBlock (AST.Block _ stmts) = do
    env <- ask
    foldM_ (\env stmt -> local (const env) $ emitStmt stmt) env stmts
    return env

emitStmt :: AST.Stmt -> IRGMonad IRGEnv
emitStmt (AST.Empty _) = ask

emitStmt (AST.BStmt _ block) = emitBlock block

emitStmt (AST.Decl _ t items) = do
    env <- ask
    foldM (\env item -> local (const env) $ declareItem item) env items
    where
        t' :: Type
        t' = convert t

        declareItem :: AST.Item -> IRGMonad IRGEnv
        declareItem item = do
            (i, val) <- unpackItem item
            reg <- newTemp
            emit $ Instr (Just reg) (IAlloca t')
            emit $ Instr Nothing (IStore t' val (TPtr t') reg)
            declareIdent i (t', reg)

        unpackItem :: AST.Item -> IRGMonad (Ident, Arg)
        unpackItem (AST.NoInit _ i) = getDefault t <&> (convert i,)
        unpackItem (AST.Init _ i e) = genExpr e <&> (convert i,) . snd


emitStmt (AST.Ass _ i e) = do
    (tVal, val) <- genExpr e
    (_, reg) <- findIdent (convert i)
    emit $ Instr Nothing (IStore tVal val (TPtr tVal) reg)
    ask

emitStmt (AST.Incr _ i) = emitIncrDecr (convert i) Add

emitStmt (AST.Decr _ i) = emitIncrDecr (convert i) Sub

emitStmt (AST.Ret _ e) = genExpr e >>= emit . Instr Nothing . uncurry IRet >> ask

emitStmt (AST.VRet _) = emit (Instr Nothing IRetV) >> ask

emitStmt (AST.Cond _ e ifStmt) = case e of
    (AST.ELitTrue _) -> emitStmt ifStmt
    (AST.ELitFalse _) -> ask
    _ -> do
        lTrue <- getLabel
        lEnd <- getLabel
        (_, cond) <- genExpr e
        emit $ Instr Nothing (IBranchCond cond lTrue lEnd)

        useBlock lTrue
        emitStmt ifStmt
        emit $ Instr Nothing (IBranch lEnd)

        useBlock lEnd
        ask

emitStmt (AST.CondElse _ e ifStmt elseStmt) = case e of
    (AST.ELitTrue _) -> emitStmt ifStmt
    (AST.ELitFalse _) -> emitStmt elseStmt
    _ -> do
        lTrue <- getLabel
        lFalse <- getLabel
        lEnd <- getLabel
        (_, cond) <- genExpr e
        emit $ Instr Nothing (IBranchCond cond lTrue lFalse)

        useBlock lTrue
        emitStmt ifStmt
        ifReturns <- use currBlock >>= blockReturns
        unless ifReturns (emit $ Instr Nothing (IBranch lEnd))

        useBlock lFalse
        emitStmt elseStmt
        elseReturns <- use currBlock >>= blockReturns
        unless elseReturns (emit $ Instr Nothing (IBranch lEnd))

        unless (ifReturns && elseReturns) do
            useBlock lEnd
        ask

emitStmt (AST.While _ e stmt) = do
    lCond <- getLabel
    lBody <- getLabel
    lEnd <- getLabel
    emit $ Instr Nothing (IBranch lCond)

    useBlock lBody
    emitStmt stmt
    emit $ Instr Nothing (IBranch lCond)

    useBlock lCond
    (tCond, cond) <- genExpr e
    emit $ Instr Nothing (IBranchCond cond lBody lEnd)

    useBlock lEnd
    ask

emitStmt (AST.SExp _ e) = genExpr e >> ask

emitIncrDecr :: Ident -> Op -> IRGMonad IRGEnv
emitIncrDecr i op = do
    (t, rhs) <- findIdent i
    lhs <- newTemp
    emit $ Instr (Just lhs) (ILoad t rhs (TPtr t))
    lhs' <- newTemp
    emit $ Instr (Just lhs') (IBinOp t lhs (AImmediate 1) op)
    emit $ Instr Nothing (IStore t lhs' (TPtr t) rhs)
    ask

genExpr :: AST.Expr -> IRGMonad (Type, Arg)
genExpr (AST.EVar _ i) = do
    lhs <- newTemp
    (t, rhs) <- findIdent (convert i)
    emit $ Instr (Just lhs) (ILoad t rhs (TPtr t))
    return (t, lhs)

genExpr (AST.ELitInt _ v) = return (TInt, AImmediate v)

genExpr (AST.ELitTrue _) = return (TBool, AImmediate 1)

genExpr (AST.ELitFalse _) = return (TBool, AImmediate 0)

genExpr (AST.EApp _ i es) = do
    vs <- mapM genExpr es
    genCall (convert i) vs

genExpr (AST.EString _ s) = do
    let s' = s ++ "\00"
    id <- getConstId s'
    lhs <- newTemp
    let tLhs = TPtr TChar
    emit $ Instr (Just lhs) (IBitcast (TPtr $ TArray (toInteger $ length s') TChar) (AConst id) tLhs)
    return (tLhs, lhs)

genExpr (AST.Neg _ e) = do
    (t, arg) <- genExpr e
    lhs <- newTemp
    emit $ Instr (Just lhs) (IBinOp t (AImmediate 0) arg Sub)
    return (t, lhs)

genExpr (AST.Not _ e) = do
    (t, arg) <- genExpr e
    lhs <- newTemp
    emit $ Instr (Just lhs) (IXor TBool arg (AImmediate 1))
    return (TBool, lhs)

genExpr (AST.EMul _ e1 op e2) = do
    (t1, a1) <- genExpr e1
    (t2, a2) <- genExpr e2
    lhs <- newTemp
    emit $ Instr (Just lhs) (IBinOp t1 a1 a2 (convert op))
    return (t1, lhs)

genExpr (AST.EAdd _ e1 op e2) = do
    (t1, a1) <- genExpr e1
    (t2, a2) <- genExpr e2
    case (t1,t2) of
        (TInt, TInt) -> do
            lhs <- newTemp
            emit $ Instr (Just lhs) (IBinOp t1 a1 a2 (convert op))
            return (t1, lhs)
        (TPtr TChar, TPtr TChar) -> do
            genCall (Ident "_concat") [(t1, a1), (t2,a2)]

genExpr (AST.ERel _ e1 op e2) = do
    (t1, a1) <- genExpr e1
    (t2, a2) <- genExpr e2
    case (t1,t2) of
        (TInt, TInt) -> do
            lhs <- newTemp
            emit $ Instr (Just lhs) (ICmpOp t1 a1 a2 (convert op))
            return (TBool, lhs)
        (TBool, TBool) -> do
            lhs <- newTemp
            emit $ Instr (Just lhs) (ICmpOp t1 a1 a2 (convert op))
            return (TBool, lhs)
        (TPtr TChar, TPtr TChar) -> do
            genCall (Ident "_strcmp") [(t1, a1), (t2,a2)]

genExpr (AST.EAnd _ e1 e2) = genBooleanOp e1 e2 False

genExpr (AST.EOr _ e1 e2) = genBooleanOp e1 e2 True

genBooleanOp :: AST.Expr -> AST.Expr -> Bool -> IRGMonad (Type, Arg)
genBooleanOp e1 e2 skipCheck = do
    (_, arg1) <- genExpr e1
    lFirst <- use currBlock
    lMid <- getLabel
    lEnd <- getLabel
    emit $ Instr Nothing (swapLabels (IBranchCond arg1) lMid lEnd)

    useBlock lMid
    (_, arg2) <- genExpr e2
    lSecond <- use currBlock
    emit $ Instr Nothing (IBranch lEnd)

    useBlock lEnd
    lhs <- newTemp
    emit $ Instr (Just lhs) (IPhi TBool [(AImmediate $ if skipCheck then 1 else 0, lFirst), (arg2, lSecond)])
    return (TBool, lhs)
    where
        swapLabels f l1 l2 = if skipCheck then f l2 l1 else f l1 l2

genCall :: Ident -> [(Type, Arg)] -> IRGMonad (Type, Arg)
genCall i args = do
    s <- findSignature i
    let t = s ^. sRetType
    if t == TVoid then do
        emit $ Instr Nothing (ICall t i args)
        return (t, AUndefined)
    else do
        lhs <- newTemp
        emit $ Instr (Just lhs) (ICall t i args)
        return (t, lhs)

class Convert a b where
    convert :: a -> b

instance Convert AST.Ident Ident where
    convert (AST.Ident i) = Ident { unIdent = TL.pack i }

instance Convert AST.Type Type where
    convert = \case
        (AST.Int _)  -> TInt
        (AST.Bool _) -> TBool
        (AST.Void _) -> TVoid
        (AST.Str _)  -> TPtr TChar

instance Convert AST.AddOp Op where
    convert = \case
        (AST.Plus _)  -> Add
        (AST.Minus _) -> Sub

instance Convert AST.MulOp Op where
    convert = \case
        (AST.Times _) -> Mul
        (AST.Div _)   -> Div
        (AST.Mod _)   -> Mod

instance Convert AST.RelOp CmpOp where
    convert = \case
        (AST.LTH _) -> LTH
        (AST.LE _)  -> LE
        (AST.GTH _) -> GTH
        (AST.GE _)  -> GE
        (AST.NE _)  -> NE
        (AST.EQU _) -> EQU

instance Convert AST.Arg (Type, Ident) where
    convert (AST.Arg _ t i) = (convert t, convert i)

class HasDefault a b where
    getDefault :: a -> IRGMonad b

instance HasDefault AST.Type Arg where
    getDefault = \case
        (AST.Int _)  -> pure $ AImmediate 0
        (AST.Bool _) -> pure $ AImmediate 0
        (AST.Str _) -> do
            let s = "\00"
            id <- getConstId s
            lhs <- newTemp
            let tLhs = TPtr TChar
            emit $ Instr (Just lhs) (IBitcast (TPtr $ TArray (toInteger $ length s) TChar) (AConst id) tLhs)
            return lhs
