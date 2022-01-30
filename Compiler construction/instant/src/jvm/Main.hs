{-# LANGUAGE TemplateHaskell #-}

module Main(main) where

import           System.Environment  (getArgs, getProgName)
import           System.Exit
import           System.FilePath     (replaceExtension, takeBaseName,
                                      takeDirectory)
import           System.IO
import           System.Process      (callProcess)

import           Instant.Abs
import           Instant.Par         (myLexer, pProgram)

import           Control.Lens
import           Control.Monad.State

import qualified Data.Map            as Map
import           Data.Maybe

data CompilerState = CompilerState { _store :: Map.Map Ident Integer }

type CompilerMonad = StateT CompilerState IO

data OpType = Add | Sub | Mul | Div

data Instr = GetStatic String String | InvokeVirtual String | IConst Integer | IStore Integer | ILoad Integer | IOp OpType | Swap

makeLenses ''CompilerState

initialCompilerState :: CompilerState
initialCompilerState = CompilerState { _store = Map.empty }

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
            let jasminFile = replaceExtension file "j"
            jasminContent <- runCompilerMonad file tree
            writeFile jasminFile jasminContent
            callProcess "java" ["-jar", "lib/jasmin.jar", jasminFile, "-d", takeDirectory jasminFile]
        Left _ -> do
            hPutStrLn stderr "parsing error"
            exitFailure

runCompilerMonad :: String -> Program -> IO String
runCompilerMonad file program = evalStateT (compile file program) initialCompilerState

compile :: String -> Program -> CompilerMonad String
compile file (Prog stmts)  = do
    instrs <- concat <$> mapM compileStmt stmts
    s <- get
    let localCnt = toInteger . Map.size $ s ^. store
    let stackSize = getStackSize instrs
    let code = map getJasmin instrs
    return . unlines $ intro file ++ (main localCnt stackSize) ++ code ++ outro
    where
        intro :: String -> [String]
        intro file = [
            ".source " ++ file,
            ".class public " ++ takeBaseName file,
            ".super java/lang/Object\n",
            ".method public <init>()V",
            "   aload_0",
            "   invokespecial java/lang/Object/<init>()V",
            "   return",
            ".end method"
            ]

        main :: Integer -> Integer -> [String]
        main localCnt stackSize = [ "\n.method public static main([Ljava/lang/String;)V",
            ".limit locals " ++ show (localCnt + 1),
            ".limit stack " ++ show stackSize
            ]

        outro :: [String]
        outro = [
            "return",
            ".end method"
            ]

compileStmt :: Stmt -> CompilerMonad [Instr]
compileStmt (SAss i e) = do
    instrs <- compileExp e
    id <- getOrDeclareIdentId i
    return $ instrs ++ [IStore id]

compileStmt (SExp e) = do
    instrs <- compileExp e
    return $ concat [
            [GetStatic "java/lang/System/out" "Ljava/io/PrintStream;"],
            instrs,
            [InvokeVirtual "java/io/PrintStream/println(I)V"]
        ]

getIdentId :: Ident -> CompilerMonad Integer
getIdentId i = do
    s <- get
    case s ^. store . at i of
        Just id -> return id
        _ -> do
            let (Ident var) = i
            liftIO $ hPutStrLn stderr ("Variable undefined: " ++ var)
            liftIO exitFailure

getOrDeclareIdentId :: Ident -> CompilerMonad Integer
getOrDeclareIdentId i = do
    s <- get
    case Map.lookup i (s ^. store) of
        Just id -> return id
        _ -> do
            let id = toInteger . Map.size $ s ^. store
            modify (& (store . at i ?~ (id + 1)))
            return $ id + 1

compileExp :: Exp -> CompilerMonad [Instr]
compileExp e = snd <$> compileExpOptimized e

compileExpOptimized :: Exp -> CompilerMonad (Integer, [Instr])
compileExpOptimized (ExpAdd e1 e2) = compileOpOptimized Add e1 e2

compileExpOptimized (ExpSub e1 e2) = compileOpOptimized Sub e1 e2

compileExpOptimized (ExpMul e1 e2) = compileOpOptimized Mul e1 e2

compileExpOptimized (ExpDiv e1 e2) = compileOpOptimized Div e1 e2

compileExpOptimized (ExpLit n)     = return (1, [IConst n])

compileExpOptimized (ExpVar i)     = do
    id <- getIdentId i
    return (1, [ILoad id])

compileOpOptimized :: OpType -> Exp -> Exp -> CompilerMonad (Integer, [Instr])
compileOpOptimized opType e1 e2 = do
    (d1, instrs1) <- compileExpOptimized e1
    (d2, instrs2) <- compileExpOptimized e2
    let instr = [IOp opType]
    return $ if d1 >= d2 then (max d1 (d2 + 1), instrs1 ++ instrs2 ++ instr) else (d2, instrs2 ++ instrs1 ++ [Swap | not $ isCommutative opType] ++ instr)
    where
        isCommutative :: OpType -> Bool
        isCommutative op = case op of
            Add -> True
            Mul -> True
            _   -> False

getStackSize :: [Instr] -> Integer
getStackSize instrs = maximum $ scanl (+) 0 (map diff instrs)
    where
        diff :: Instr -> Integer
        diff instr = case instr of
            GetStatic _ _   -> 1
            InvokeVirtual _ -> -2
            IConst _        -> 1
            IStore _        -> -1
            ILoad _         -> 1
            IOp _           -> -1
            Swap            -> 0

getJasmin :: Instr -> String
getJasmin instr = case instr of
    GetStatic s1 s2     -> "getstatic " ++ s1 ++ " " ++ s2
    InvokeVirtual s     -> "invokevirtual " ++ s
    IConst n | n <= 5   -> "iconst_" ++ show n
    IConst n | n <= 128 -> "bipush " ++ show n
    IConst n            -> "ldc " ++ show n
    IStore i | i <= 3   -> "istore_" ++ show i
    IStore i            -> "istore " ++ show i
    ILoad i | i <= 3    -> "iload_" ++ show i
    ILoad i             -> "iload " ++ show i
    IOp Add             -> "iadd"
    IOp Sub             -> "isub"
    IOp Mul             -> "imul"
    IOp Div             -> "idiv"
    Swap                -> "swap"

