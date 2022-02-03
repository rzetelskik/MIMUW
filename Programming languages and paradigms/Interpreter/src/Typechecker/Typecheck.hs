module Typechecker.Typecheck where

import           Grammar.Abs

import           Common.Exception
import           Common.TypeUtils
import           Typechecker.Environment
import           Typechecker.EnvironmentUtils
import           Typechecker.Exception
import           Typechecker.Monad

import           Control.Lens                 hiding (Empty)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Maybe                   (isNothing)

-- Program
typecheckProgram :: Program -> TypecheckerMonad ()
typecheckProgram (Prog _ defs) = do
    env <- typecheckMultipleTopDefs defs
    local (const env) typecheckMain
    where
        typecheckMain :: TypecheckerMonad ()
        typecheckMain = do
            t <- extractType NoPos (Ident "main") `catchError` (\_ -> throwPosError NoPos MainUndeclared)
            unless (t === FuncLit NoPos []) (throwPosError NoPos MainWrongType)

-- TopDefs
typecheckMultipleTopDefs :: [TopDef] -> TypecheckerMonad TEnv
typecheckMultipleTopDefs defs = do
    env <- ask
    env' <- foldM (\env def -> local (const env) $ typecheckTopDef def) env defs
    foldM (\env def -> local (const env) (typecheckBody def)) env' defs
    where
        typecheckBody :: TopDef -> TypecheckerMonad TEnv
        typecheckBody (GlobalDcl _ _) = ask
        typecheckBody (FuncDef _ _ args block) = typecheckFuncBlock args block Nothing
        typecheckBody (FuncDefT _ _ args t block) = typecheckFuncBlock args block (Just t)

        typecheckFuncBlock :: [Arg] -> Block -> Maybe Type -> TypecheckerMonad TEnv
        typecheckFuncBlock args block mt = do
            let flatArgs = flattenArgs args
            local (& (retType .~ mt)) (typecheckFunc flatArgs block)

typecheckTopDef :: TopDef -> TypecheckerMonad TEnv
typecheckTopDef (GlobalDcl _ decl) = typecheckDecl decl

typecheckTopDef (FuncDef pos i args block) = do
    let flatArgs = flattenArgs args
    declareIdent pos i (FuncLit NoPos $ map snd flatArgs)

typecheckTopDef (FuncDefT pos i args t block) = do
    let flatArgs = flattenArgs args
    declareIdent pos i (FuncLitT NoPos (map snd flatArgs) t)


-- Declarations
typecheckDecl :: Decl -> TypecheckerMonad TEnv
typecheckDecl (Dcl pos idents t) = do
    let types =replicate (length idents) t
    typecheckDecl' pos idents types

typecheckDecl (DclInit pos idents exprs) = do
    types <- mapM typecheckExpr exprs
    mapM_ checkInternalType types
    typecheckDecl' pos idents types
    where
        checkInternalType :: Type -> TypecheckerMonad ()
        checkInternalType t = case t of
            Ptr pos (Void _) -> throwPosError pos UntypedNil
            Void _           -> throwPosError pos NonRetFuncAsValue
            _                -> return ()

typecheckDecl (DclInitT pos idents t exprs) = do
    types <- mapM typecheckExpr exprs

    mapM_ (\t' -> unless (t === t') (throwPosError (hasPosition t') $ TypeMismatch t' t)) types

    typecheckDecl' pos idents (replicate (length types) t)

typecheckDecl' :: BNFC'Position -> [Ident] -> [Type] -> TypecheckerMonad TEnv
typecheckDecl' pos idents types = do
    unless (length idents == length types) (throwPosError pos AssignmentMismatch)

    env <- ask
    foldM (\env (i, t) -> local (const env) (declareIdent (hasPosition t) i t)) env (zip idents types)


-- Statements
typecheckMultipleStmts :: [Stmt] -> TypecheckerMonad TEnv
typecheckMultipleStmts stmts = do
    env <- ask
    foldM (\env stmt -> local (const env) (typecheckStmt stmt)) env stmts

typecheckStmt :: Stmt -> TypecheckerMonad TEnv
typecheckStmt (Empty _) = ask

typecheckStmt (BStmt _ block) = typecheckBlock block

typecheckStmt (DStmt _ decl) = typecheckDecl decl

typecheckStmt (Ass pos lexprs rexprs) = do
    unless (length lexprs == length rexprs) (throwPosError pos AssignmentMismatch)

    ltypes <- mapM typecheckExpr lexprs
    rtypes <- mapM typecheckExpr rexprs

    mapM_ (\(l,r) -> unless (l === r) (throwPosError (hasPosition l) $ TypeMismatch r l)) (zip ltypes rtypes)
    ask

typecheckStmt (For _ s1 expr s2 block) = do
    env <- local (& (scope +~ 1)) (typecheckStmt s1)
    t <- local (const env) (typecheckExpr expr)
    unless (t === Bool NoPos) (throwPosError (hasPosition t) $ NonBoolCondition t)

    local (const env) (typecheckBlock block)
    local (const env) (typecheckStmt s2)
    ask

typecheckStmt (ForExpr pos expr block) = typecheckStmt (For pos (Empty NoPos) expr (Empty NoPos) block)

typecheckStmt (Incr pos i) = do
    t <- extractType pos i
    case t of
        Int _ -> ask
        _     -> throwPosError pos InvalidOperation

typecheckStmt (Decr pos i) = do
    t <- extractType pos i
    case t of
        Int _ -> ask
        _     -> throwPosError pos InvalidOperation

typecheckStmt (Ret pos e) = do
    t <- typecheckExpr e
    env <- ask

    case env ^. retType of
        Just t' -> if t' === t then return $ env & (ret .~ True) else throwPosError (hasPosition t) $ TypeMismatch t t'
        _       -> throwPosError pos ReturnTooMany

typecheckStmt (VoidRet pos) = do
    env <- ask

    case env ^. retType of
        Nothing -> return $ env & (ret .~ True)
        _       -> throwPosError pos ReturnNotEnough

typecheckStmt (Cond pos e block) = typecheckStmt (CondElse pos e block (Blk NoPos []))

typecheckStmt (CondElse pos e blockIf blockElse) = do
    t <- typecheckExpr e
    unless (t === Bool NoPos) (throwPosError (hasPosition t) $ NonBoolCondition t)

    envIf <- typecheckBlock blockIf
    envElse <- typecheckBlock blockElse
    asks (& ret .~ (envIf ^. ret && envElse ^. ret))

typecheckStmt (Print _ exprs) = do
    types <- mapM typecheckExpr exprs
    mapM_ typecheckPrintable types
    ask
    where
        typecheckPrintable :: Type -> TypecheckerMonad ()
        typecheckPrintable t = case t of
            Void _      -> throwPosError (hasPosition t) NonRetFuncAsValue
            FuncLit _ _ -> throwPosError (hasPosition t) FuncNotCalled
            FuncLitT{}  -> throwPosError (hasPosition t) FuncNotCalled
            _           -> return ()

typecheckStmt (SExp _ e) = typecheckExpr e >> ask


-- Expressions
typecheckExpr :: Expr -> TypecheckerMonad Type
typecheckExpr (EApp pos i exprs) = do
    t <- extractType pos i
    typecheckFuncApp i (repositionType t pos) exprs

typecheckExpr (ELitFun pos args block) = do
    let flatArgs = flattenArgs args
    local (& (retType .~ Nothing)) (typecheckFunc flatArgs block)
    return $ FuncLit pos (map snd flatArgs)


typecheckExpr (ELitFunT pos args t block) = do
    let flatArgs = flattenArgs args
    local (& (retType ?~ t)) (typecheckFunc flatArgs block)
    return $ FuncLitT pos (map snd flatArgs) t

typecheckExpr (ELitFunApp pos f exprs) = do
    t <- typecheckExpr f
    typecheckFuncApp (Ident "func literal") (repositionType t pos) exprs

typecheckExpr (EVar pos i) = do
    t <- extractType pos i
    return $ repositionType t pos

typecheckExpr (ELitInt pos _) = return $ Int pos

typecheckExpr (ELitTrue pos) = return $ Bool pos

typecheckExpr (ELitFalse pos) = return $ Bool pos

typecheckExpr (EString pos _) = return $ Str pos

typecheckExpr (ELitNil pos) = return $ Ptr pos (Void NoPos)

typecheckExpr (Ref pos e) = (typecheckExpr >=> return . Ptr pos) e

typecheckExpr (Deref pos e) = do
    t <- typecheckExpr e
    case t of
        (Ptr _ t') -> return t'
        _          -> throwPosError pos $ InvalidIndirect t

typecheckExpr (Neg pos e) = do
    t <- typecheckExpr e
    case t of
        Int _ -> return $ Int NoPos
        _     -> throwPosError pos InvalidOperation

typecheckExpr (Not pos e) = do
    t <- typecheckExpr e
    case t of
        Bool _ -> return $ Bool NoPos
        _      -> throwPosError pos InvalidOperation

typecheckExpr (EMul pos e1 _ e2) = typecheckIntOp pos e1 e2

typecheckExpr (EAdd pos e1 _ e2) = typecheckIntOp pos e1 e2

typecheckExpr (ERel pos e1 _ e2) = do
    t1 <- typecheckExpr e1
    t2 <- typecheckExpr e2
    case (t1, t2) of
        (Int _, Int _) -> return $ Bool pos
        (Str _, Str _) -> return $ Bool pos
        _              -> throwPosError pos InvalidOperation

typecheckExpr (EAnd pos e1 e2) = typecheckBoolOp pos e1 e2

typecheckExpr (EOr pos e1 e2) = typecheckBoolOp pos e1 e2

typecheckIntOp :: BNFC'Position -> Expr -> Expr -> TypecheckerMonad Type
typecheckIntOp pos e1 e2 = do
    t1 <- typecheckExpr e1
    t2 <- typecheckExpr e2
    case (t1, t2) of
        (Int _, Int _) -> return $ Int pos
        _              -> throwPosError pos InvalidOperation

typecheckBoolOp :: BNFC'Position -> Expr -> Expr -> TypecheckerMonad Type
typecheckBoolOp pos e1 e2 = do
    t1 <- typecheckExpr e1
    t2 <- typecheckExpr e2
    case (t1, t2) of
        (Bool _, Bool _) -> return $ Bool pos
        _                -> throwPosError pos InvalidOperation

typecheckBlock :: Block -> TypecheckerMonad TEnv
typecheckBlock block = local (& (scope +~ 1)) (typecheckBlockNoIncrease block)

typecheckBlockNoIncrease :: Block -> TypecheckerMonad TEnv
typecheckBlockNoIncrease (Blk _ stmts) = typecheckMultipleStmts stmts

typecheckFunc :: [(Ident,Type)] -> Block -> TypecheckerMonad TEnv
typecheckFunc flatArgs block = do
    env <- ask
    env' <- foldM (\env arg -> local (const env) (uncurry (declareIdent NoPos) arg)) (env & (scope +~ 1)) flatArgs
    env'' <- local (const env') (typecheckBlockNoIncrease block)
    unless (isNothing (env'' ^. retType) || env'' ^. ret ) (throwPosError (hasPosition block) MissingReturn)
    return env

typecheckFuncApp :: Ident -> Type -> [Expr] -> TypecheckerMonad Type
typecheckFuncApp i t exprs = do
    exprTs <- mapM typecheckExpr exprs
    case t of
        FuncLitT _ argTs t -> typecheckArgs argTs t exprTs
        FuncLit _ argTs    -> typecheckArgs argTs (Void NoPos) exprTs
        t                  -> throwPosError (hasPosition t) $ CallNonFunction i t
    where
        typecheckArgs :: [Type] -> Type -> [Type] -> TypecheckerMonad Type
        typecheckArgs argTs t exprTs = do
            mapM_ (\(argT,exprT) -> unless (argT === exprT) (throwPosError (hasPosition exprT) $ TypeMismatch exprT argT)) (zip argTs exprTs)
            return t
