{-# LANGUAGE LambdaCase #-}

module Frontend.SemanticAnalysis where

import           Latte.Abs

import           Frontend.Environment
import           Frontend.EnvironmentUtils
import           Frontend.Exception
import           Frontend.Monad
import           Frontend.TypeUtils


import           Control.Lens                      hiding (Empty)
import           Control.Monad.Except
import           Control.Monad.Reader


-- Program
analyseProgram :: Program -> SemanticAnalysisMonad ()
analyseProgram (Program _ defs) = do
    env <- analyseMultipleTopDefs defs
    local (const env) analyseMain
    where
        analyseMain :: SemanticAnalysisMonad ()
        analyseMain = do
            t <- extractType NoPos (Ident "main") `catchError` (\_ -> throwPosError NoPos MainUndeclared)
            unless (t === Fun NoPos (Int NoPos) []) (throwPosError NoPos MainWrongType)

-- TopDefs
analyseMultipleTopDefs :: [TopDef] -> SemanticAnalysisMonad Env
analyseMultipleTopDefs defs = do
    env <- ask
    env' <- foldM (\env def -> local (const env) $ analyseTopDef def) env defs
    foldM (\env def -> local (const env) (analyseBody def)) env' defs
    where
        analyseBody :: TopDef -> SemanticAnalysisMonad Env
        analyseBody (FnDef _ t _ args block) = analyseFuncBlock args block t

        analyseFuncBlock :: [Arg] -> Block -> Type -> SemanticAnalysisMonad Env
        analyseFuncBlock args block t = do
            local (& (retType .~ t)) (analyseFunc args block)

analyseTopDef :: TopDef -> SemanticAnalysisMonad Env
analyseTopDef (FnDef pos t i args block) = do
    declareIdent pos i (Fun NoPos t (map (\(Arg _ t _) -> t) args))

analyseFunc :: [Arg] -> Block -> SemanticAnalysisMonad Env
analyseFunc args block = do
    env <- ask
    env' <- foldM (\env (Arg pos t i) -> local (const env) (declareIdent pos i t)) (env & (scope +~ 1)) args
    env'' <- local (const env') (analyseBlockNoIncrease block)
    unless (isVoid (env'' ^. retType) || env'' ^. ret ) (throwPosError (hasPosition block) MissingReturn)
    return env

analyseBlock :: Block -> SemanticAnalysisMonad Env
analyseBlock block = local (& (scope +~ 1)) (analyseBlockNoIncrease block)

analyseBlockNoIncrease :: Block -> SemanticAnalysisMonad Env
analyseBlockNoIncrease (Block _ stmts) = analyseMultipleStmts stmts

-- Statements
analyseMultipleStmts :: [Stmt] -> SemanticAnalysisMonad Env
analyseMultipleStmts stmts = do
    env <- ask
    foldM (\env stmt -> local (const env) (analyseStmt stmt)) env stmts

analyseStmt :: Stmt -> SemanticAnalysisMonad Env
analyseStmt (Empty _) = ask

analyseStmt (BStmt _ block) = analyseBlock block

analyseStmt (Decl _ t items) = do
    ts <- mapM analyseExpr [e | Init _ _ e <- items]
    mapM_ (\t' -> unless (t === t') (throwPosError (hasPosition t') $ TypeMismatch t' t)) ts

    env <- ask
    foldM (\env i -> local (const env) (declareIdent (hasPosition t) i t)) env (map identFromItem items)
        where
            identFromItem :: Item -> Ident
            identFromItem (NoInit _ i) = i
            identFromItem (Init _ i _) = i

analyseStmt (Ass pos i e) = do
    lt <- extractType pos i
    rt <- analyseExpr e

    unless (lt === rt) (throwPosError pos $ TypeMismatch rt lt)
    ask

analyseStmt (Incr pos i) = analyseIncrDecr pos i

analyseStmt (Decr pos i) = analyseIncrDecr pos i

analyseStmt (Ret pos e) = do
    t <- analyseExpr e
    env <- ask

    let t' = env ^. retType
    when (isVoid t') (throwPosError pos NonVoidReturn)
    unless (t' === t) (throwPosError (hasPosition t) $ TypeMismatch t t')
    return $ env & (ret .~ True)

analyseStmt (VRet pos) = do
    env <- ask
    unless (isVoid $ env ^. retType) (throwPosError pos VoidReturn)
    return $ env & (ret .~ True)

analyseStmt (Cond pos e stmt) = analyseStmt (CondElse pos e stmt (Empty NoPos))

analyseStmt (CondElse pos e ifStmt elseStmt) = do
    envIf <- analyseStmt ifStmt
    envElse <- analyseStmt elseStmt

    case e of
        ELitTrue _  -> asks (& ret .~ (envIf ^. ret))
        ELitFalse _ -> asks (& ret .~ (envElse ^. ret))
        _           -> do
            t <- analyseExpr e
            unless (t === Bool NoPos) (throwPosError (hasPosition t) $ NonBoolCondition t)

            asks (& ret .~ (envIf ^. ret && envElse ^. ret))

analyseStmt (While _ e stmt) = do
    t <- analyseExpr e
    unless (t === Bool NoPos) (throwPosError (hasPosition t) $ NonBoolCondition t)

    analyseStmt stmt

analyseStmt (SExp _ e) = analyseExpr e >> ask

analyseIncrDecr :: BNFC'Position -> Ident -> SemanticAnalysisMonad Env
analyseIncrDecr pos i = extractType pos i >>= \case
    Int _ -> ask
    _     -> throwPosError pos InvalidOperation

-- Expressions
analyseExpr :: Expr -> SemanticAnalysisMonad Type
analyseExpr (EVar pos i) = do
    t <- extractType pos i
    return $ repositionType t pos

analyseExpr (ELitInt pos _) = return $ Int pos

analyseExpr (ELitTrue pos) = return $ Bool pos

analyseExpr (ELitFalse pos) = return $ Bool pos

analyseExpr (EApp pos i es) = do
    t <- extractType pos i
    analyseFuncApp pos i (repositionType t pos) es

analyseExpr (EString pos _) = return $ Str pos

analyseExpr (Neg pos e) = analyseExpr e >>= \case
    Int _ -> return $ Int NoPos
    _     -> throwPosError pos InvalidOperation

analyseExpr (Not pos e) = analyseExpr e >>= \case
    Bool _ -> return $ Bool NoPos
    _      -> throwPosError pos InvalidOperation

analyseExpr (EMul pos e1 _ e2) = do
    t1 <- analyseExpr e1
    t2 <- analyseExpr e2
    case (t1, t2) of
        (Int _, Int _) -> return $ Int pos
        _              -> throwPosError pos InvalidOperation

analyseExpr (EAdd pos e1 _ e2) = do
    t1 <- analyseExpr e1
    t2 <- analyseExpr e2
    case (t1, t2) of
        (Int _, Int _) -> return $ Int pos
        (Str _, Str _) -> return $ Str pos
        _              -> throwPosError pos InvalidOperation

analyseExpr (ERel pos e1 op e2) = do
    t1 <- analyseExpr e1
    t2 <- analyseExpr e2
    case (t1, t2) of
        (Int _, Int _)   -> return $ Bool pos
        (Str _, Str _)   -> case op of
            (EQU _) -> return $ Bool pos
            (NE _)  -> return $ Bool pos
            _       -> throwPosError pos InvalidOperation
        (Bool _, Bool _) -> case op of
            (EQU _) -> return $ Bool pos
            (NE _)  -> return $ Bool pos
            _       -> throwPosError pos InvalidOperation
        _                -> throwPosError pos InvalidOperation

analyseExpr (EAnd pos e1 e2) = analyseBoolOp pos e1 e2

analyseExpr (EOr pos e1 e2) = analyseBoolOp pos e1 e2

analyseFuncApp :: BNFC'Position -> Ident -> Type -> [Expr] -> SemanticAnalysisMonad Type
analyseFuncApp pos i t es = do
    eTs <- mapM analyseExpr es
    case t of
        Fun _ t argTs -> analyseArgs t argTs eTs
        t             -> throwPosError (hasPosition t) $ CallNonFunction i t
    where
        analyseArgs :: Type -> [Type] -> [Type] -> SemanticAnalysisMonad Type
        analyseArgs t argTs eTs = do
            unless (length argTs == length eTs) (throwPosError pos $ CallBadArgNum i eTs argTs)
            mapM_ (\(argT,eT) -> unless (argT === eT) (throwPosError (hasPosition eT) $ TypeMismatch eT argT)) (zip argTs eTs)
            return t

analyseBoolOp :: BNFC'Position -> Expr -> Expr -> SemanticAnalysisMonad Type
analyseBoolOp pos e1 e2 = do
    t1 <- analyseExpr e1
    t2 <- analyseExpr e2
    case (t1, t2) of
        (Bool _, Bool _) -> return $ Bool pos
        _                -> throwPosError pos InvalidOperation
