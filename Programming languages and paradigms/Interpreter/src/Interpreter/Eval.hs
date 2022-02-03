module Interpreter.Eval where

import           Grammar.Abs

import           Common.Exception
import           Common.TypeUtils
import           Interpreter.Environment
import           Interpreter.EnvironmentUtils
import           Interpreter.Exception
import           Interpreter.Monad

import           Control.Lens                 hiding (Empty)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                     as Map

-- Program
evalProgram :: Program -> InterpreterMonad ()
evalProgram (Prog _ defs) = do
    env <- evalMultipleTopDefs defs
    void $ local (const env) (evalExpr (EApp NoPos (Ident "main") []))


-- TopDefs
evalMultipleTopDefs :: [TopDef] -> InterpreterMonad Env
evalMultipleTopDefs defs = do
    env <- ask
    env' <- foldM (\env def -> local (const env) $ evalTopDef def) env defs
    modify(& imap(\_ v -> updateTopEnv env' v))
    return env'
    where
        updateTopEnv :: Env -> MVal -> MVal
        updateTopEnv env (FuncDefV args t block _) = FuncDefV args t block env
        updateTopEnv _ mval                        = mval

evalTopDef :: TopDef -> InterpreterMonad Env
evalTopDef (GlobalDcl _ decl)          = evalDecl decl

evalTopDef (FuncDef _ i args block)    = evalTopDef' i args (Void NoPos) block

evalTopDef (FuncDefT _ i args t block) = evalTopDef' i args t block

evalTopDef' :: Ident -> [Arg] -> Type -> Block -> InterpreterMonad Env
evalTopDef' i args t block = declareIdent i (FuncDefV (flattenArgs args) t block initEnv)


-- Declarations
evalDecl :: Decl -> InterpreterMonad Env
evalDecl (Dcl _ idents t) = do
    mval <- defaultMVal t
    let mvals = replicate (length idents) mval
    evalDecl' idents mvals

evalDecl (DclInit _ idents exprs) = mapM evalExpr exprs >>= evalDecl' idents

evalDecl (DclInitT _ idents _ exprs) = mapM evalExpr exprs >>= evalDecl' idents

evalDecl' :: [Ident] -> [MVal] -> InterpreterMonad Env
evalDecl' idents mvals = do
    env <- ask
    foldM (\env (i, mval) -> local (const env) (declareIdent i mval)) env (zip idents mvals)


-- Statements
evalMultipleStmts :: [Stmt] -> InterpreterMonad Env
evalMultipleStmts stmts = do
    env <- ask
    foldM (\env stmt -> local (const env) (evalStmt stmt)) env stmts

evalStmt :: Stmt -> InterpreterMonad Env
evalStmt (Empty _) = ask

evalStmt (BStmt _ block) = evalBlock block

evalStmt (DStmt _ decl) = evalDecl decl

evalStmt (Ass _ lexprs rexprs) = do
    ls <- mapM (evalLoc >=> unpackPtr) lexprs
    rs <- mapM evalExpr rexprs

    mapM_ ( uncurry updateAtLoc) (zip ls rs)
    ask
        where
            unpackPtr :: MVal -> InterpreterMonad Loc
            unpackPtr (PtrV l) = return l

evalStmt (For _ s1 expr s2 block) = do
    env <- evalStmt s1
    local (const env) loop
    where
        loop :: InterpreterMonad Env
        loop = do
            env <- ask
            BoolV b <- evalExpr expr
            if b then do
                evalBlock block
                evalStmt s2
                loop
            else
                return env

evalStmt (ForExpr pos expr block) = evalStmt (For pos (Empty NoPos) expr (Empty NoPos) block)

evalStmt (Incr _ i) = transformIntV i (+1) >> ask

evalStmt (Decr _ i) = transformIntV i (\n -> n - 1) >> ask

evalStmt (Ret pos e) = evalExpr e >>= throwPosError pos . Return

evalStmt (VoidRet pos) = throwPosError pos $ Return VoidV

evalStmt (Cond pos e block) = evalStmt (CondElse pos e block (Blk NoPos []))

evalStmt (CondElse _ e blockIf blockElse) = do
    (BoolV cond) <- evalExpr e
    evalBlock $ if cond then blockIf else blockElse

evalStmt (Print _ exprs) = do
    mvals <- mapM evalExpr exprs
    mapM_ (liftIO . putStr . show) mvals
    ask

evalStmt (SExp _ e) = evalExpr e >> ask


-- Expresions
evalExpr :: Expr -> InterpreterMonad MVal
evalExpr (EApp _ i exprs) = do
    mval <- find i
    evalFuncApp i mval exprs

evalExpr (ELitFunT _ args t block) = asks (FuncDefV (flattenArgs args) t block)

evalExpr (ELitFun pos args block) = evalExpr (ELitFunT pos args (Void NoPos) block)

evalExpr (ELitFunApp _ f exprs) = do
    mval <- evalExpr f
    evalFuncApp (Ident "func literal") mval exprs

evalExpr (EVar _ i) = find i

evalExpr (ELitInt _ v) = return $ IntV v

evalExpr (ELitTrue _) = return $ BoolV True

evalExpr (ELitFalse _) = return $ BoolV False

evalExpr (EString _ s) = return $ StringV s

evalExpr (ELitNil _) = return NilV

evalExpr (Ref _ e) = evalLoc e

evalExpr (Deref pos e) = do
    mval <- evalExpr e
    case mval of
        NilV   -> throwPosError pos NilDereference
        PtrV l -> extractMVal l

evalExpr (Neg _ e) = do
    (IntV i) <- evalExpr e
    return $ IntV (negate i)

evalExpr (Not _ e) = do
    (BoolV b) <- evalExpr e
    return $ BoolV (not b)

evalExpr (EMul _ e1 op e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    case (op, v1, v2) of
        (Div pos, _, IntV 0)  -> throwPosError pos DivisionByZero
        (Mod pos, _, IntV 0)  -> throwPosError pos DivisionByZero
        (_, IntV i1, IntV i2) -> return $ IntV $ translateMulOp op i1 i2
    where
        translateMulOp :: Integral a => MulOp -> (a -> a -> a)
        translateMulOp op = case op of
            Times _ -> (*)
            Div _   -> div
            Mod _   -> mod

evalExpr (EAdd _ e1 op e2) = do
    IntV i1 <- evalExpr e1
    IntV i2 <- evalExpr e2
    return $ IntV $ translateAddOp op i1 i2
    where
        translateAddOp :: Num a => AddOp -> (a -> a -> a)
        translateAddOp op = case op of
            Plus _  -> (+)
            Minus _ -> (-)

evalExpr (ERel _ e1 op e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    case (v1, v2) of
        (IntV i1, IntV i2)       -> return $ BoolV $ translateRelOp op i1 i2
        (StringV s1, StringV s2) -> return $ BoolV $ translateRelOp op s1 s2
    where
        translateRelOp :: Ord a => RelOp -> (a -> a -> Bool)
        translateRelOp op = case op of
            LTH _ -> (<)
            LE _  -> (<=)
            GTH _ -> (>)
            GE _  -> (>=)
            EQU _ -> (==)
            NE _  -> (/=)

evalExpr (EAnd _ e1 e2) = do
    BoolV b1 <- evalExpr e1
    BoolV b2 <- evalExpr e2
    return $ BoolV $ b1 && b2

evalExpr (EOr _ e1 e2) = do
    BoolV b1 <- evalExpr e1
    BoolV b2 <- evalExpr e2
    return $ BoolV $ b1 || b2

evalBlock :: Block -> InterpreterMonad Env
evalBlock (Blk _ stmts) = do
    env <- ask
    local (const env) (evalMultipleStmts stmts)
    return env

evalLoc :: Expr -> InterpreterMonad MVal
evalLoc (Deref _ e) = evalExpr e
evalLoc (EVar _ i)  = PtrV <$> extractLoc i
evalLoc e           = throwPosError (hasPosition e) CantTakeAddr

evalFuncApp :: Ident -> MVal -> [Expr] -> InterpreterMonad MVal
evalFuncApp i (FuncDefV args t block env) es = do
    vals <- mapM evalExpr es
    env' <- foldM (\env (FuncArg i, mval) -> local (const env) (declareIdent i mval)) env (zip args vals)

    local (const env') (evalBlock block >> return VoidV) `catchError` catchReturn t
        where
            catchReturn :: Type -> TracedRuntimeException -> InterpreterMonad MVal
            catchReturn t (pos, except) =
                case except of
                    Return mval -> return mval
                    _           -> throwPosError pos except

