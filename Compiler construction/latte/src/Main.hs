module Main where

import           Data.Map            as Map

import           System.Environment  (getArgs, getProgName)
import           System.Exit         (exitFailure, exitSuccess)
import           System.FilePath     (dropExtension, replaceExtension)
import           System.IO
import           System.Process      (callProcess)

import qualified Data.Text.Lazy.IO   as TL

import           Latte.Abs
import           Latte.Par           (myLexer, pProgram)

import           Control.Lens
import           Control.Monad       (unless)
import           Data.Maybe          (fromJust, isNothing)

import           Backend.LLVM
import           Backend.Opt.Run
import           Backend.Run
import qualified Frontend.Exception
import           Frontend.Run

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
                hPutStr h $ unlines [
                    "Usage: " ++ exec ++ " path...",
                    "       " ++ exec ++ " --help"
                    ]

processFile :: FilePath -> IO ()
processFile filename = do
    content <- readFile filename
    case pProgram (myLexer content) of
        Right tree -> case runSemanticAnalysisMonad tree of
            Right _ -> do
                hPutStrLn stderr "OK"
                let llvmFile = replaceExtension filename "ll"
                llvmCode <- compile tree >>= opt <&> toLLVM
                TL.writeFile llvmFile llvmCode
                callProcess "llvm-as" [llvmFile, "-o", replaceExtension filename "bc"]
                callProcess "llvm-link" ["-o", replaceExtension filename "bc", replaceExtension filename "bc", "lib/runtime.bc"]
                exitSuccess
            Left (pos, err) -> do
                exitError pos $ Frontend.Exception.getExceptionMessage err
        Left s -> exitError BNFC'NoPosition s

exitError :: BNFC'Position -> String -> IO ()
exitError pos err = do
    hPutStrLn stderr "ERROR"

    unless (isNothing pos) (let (ln, col) = fromJust pos in hPutStr stderr (show ln ++ ":" ++ show col ++ ": "))
    hPutStrLn stderr err
    exitFailure
