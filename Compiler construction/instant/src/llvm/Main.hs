{-# LANGUAGE TemplateHaskell #-}

module Main(main) where

import           System.Environment  (getArgs, getProgName)
import           System.Exit         (exitFailure, exitSuccess)
import           System.FilePath     (replaceExtension)
import           System.IO
import           System.Process      (callProcess)

import           Text.Printf

import           Instant.Abs
import           Instant.Par         (myLexer, pProgram)

import           Control.Lens
import           Control.Monad.State

import qualified Data.Map            as Map
import           Data.Maybe


data Arg = Reg Integer | Val Integer

instance PrintfArg Arg where
    formatArg (Reg r) = formatArg ("%" ++ show r)
    formatArg (Val i) = formatArg (show i)

data OpType = Add | Sub | Mul | Div

instance PrintfArg OpType where
    formatArg Add = formatArg "add"
    formatArg Sub = formatArg "sub"
    formatArg Mul = formatArg "mul"
    formatArg Div = formatArg "sdiv"

data Instr = Output Arg | Op Arg OpType Arg Arg | Alloc Arg | Store Arg Arg | Load Arg Arg

data CompilerState = CompilerState { _stack :: [Instr], _store :: Map.Map Ident Arg, _count :: Integer }

type CompilerMonad = StateT CompilerState IO

makeLenses ''CompilerState

initialCompilerState :: CompilerState
initialCompilerState = CompilerState { _stack = [], _store = Map.empty, _count = 1 }

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> do
            usage stdout
            exitSuccess
        [] -> do
            usage stderr
            exitFailure
        fs -> mapM_ processFile fs
        where
            usage :: Handle -> IO ()
            usage h = do
                exec <- getProgName
                hPutStrLn h $ unlines [
                    "Usage: " ++ exec ++ " path...",
                    "       " ++ exec ++ " --help"
                    ]

processFile :: FilePath -> IO ()
processFile file = do
    content <- readFile file
    case pProgram (myLexer content) of
        Right tree -> do
            let llvmFile = replaceExtension file "ll"
            llvmContent <- runCompilerMonad tree
            writeFile llvmFile llvmContent
            callProcess "llvm-as" [llvmFile, "-o", replaceExtension file "bc"]
        Left _ -> do
            hPutStrLn stderr "parsing error"
            exitFailure

runCompilerMonad :: Program -> IO String
runCompilerMonad program = evalStateT (compile program) initialCompilerState

compile :: Program -> CompilerMonad String
compile (Prog stmts) = do
    mapM_ compileStmt stmts
    s <- get
    let code = map getLLVM (reverse $ s ^. stack)
    return . unlines $ intro ++ code ++ outro

intro :: [String]
intro = ["declare dso_local void @printf(i8*, ...)",
        "@.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\"",
        "define dso_local i32 @main() {"]

outro :: [String]
outro = ["ret i32 0", "}"]

compileStmt :: Stmt -> CompilerMonad ()
compileStmt (SAss i e) = do
    expArg <- compileExp e
    iReg <- getOrDeclareIdentReg i
    pushInstr $ Store expArg iReg

compileStmt (SExp e) = do
    val <- compileExp e
    pushInstr $ Output val

compileExp :: Exp -> CompilerMonad Arg
compileExp (ExpAdd e1 e2) = compileOp Add e1 e2

compileExp (ExpSub e1 e2) = compileOp Sub e1 e2

compileExp (ExpMul e1 e2) = compileOp Mul e1 e2

compileExp (ExpDiv e1 e2) = compileOp Div e1 e2

compileExp (ExpLit n) = return $ Val n

compileExp (ExpVar i) = do
    lReg <- getNextReg
    rReg <- getIdentReg i
    pushInstr $ Load lReg rReg
    return lReg

compileOp :: OpType -> Exp -> Exp -> CompilerMonad Arg
compileOp opType e1 e2 = do
    regE1 <- compileExp e1
    regE2 <- compileExp e2
    resReg <- getNextReg
    pushInstr $ Op resReg opType regE1 regE2
    return resReg

getNextReg :: CompilerMonad Arg
getNextReg = do
    s <- get
    let reg = s ^. count
    put (s & (count %~ (+1)))
    return $ Reg reg

getIdentReg :: Ident -> CompilerMonad Arg
getIdentReg i = do
    s <- get
    case s ^. store . at i of
        Just reg -> return reg
        _ -> do
            let (Ident var) = i
            liftIO $ hPutStrLn stderr ("Variable undefined: " ++ var)
            liftIO exitFailure


getOrDeclareIdentReg :: Ident -> CompilerMonad Arg
getOrDeclareIdentReg i = do
    s <- get
    case Map.lookup i (s ^. store) of
        Just reg -> return reg
        _        -> do
                    reg <- getNextReg
                    modify (& (store . at i ?~ reg))
                    pushInstr $ Alloc reg
                    return reg


pushInstr :: Instr -> CompilerMonad ()
pushInstr instr = do
    s <- get
    put (s & (stack %~ (instr:)))

getLLVM :: Instr -> String
getLLVM (Output arg) = printf "call void (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i64 0, i64 0), i32 %s)" arg
getLLVM (Op resReg opType lReg rReg) = printf "%s = %s i32 %s, %s" resReg opType lReg rReg
getLLVM (Alloc reg) = printf "%s = alloca i32" reg
getLLVM (Store val reg) = printf "store i32 %s, i32* %s" val reg
getLLVM (Load lReg rReg) = printf "%s = load i32, i32* %s" lReg rReg
