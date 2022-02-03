module Main where

import           Data.Map              as Map

import           System.Environment    (getArgs)
import           System.Exit           (exitFailure, exitSuccess)
import           System.IO             (getContents, hPutStr, hPutStrLn, stderr)

import           Grammar.Abs
import           Grammar.Par           (myLexer, pProgram)

import qualified Typechecker.Exception
import           Typechecker.Run

import qualified Interpreter.Exception
import           Interpreter.Run

import           Control.Monad         (unless)
import           Data.Maybe            (fromJust, isNothing)

type Err = Either String

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> getContents >>= parse
        fs -> mapM_ parseFile fs

parseFile :: FilePath -> IO ()
parseFile filename = readFile filename >>= parse

parse :: String -> IO ()
parse s = case pProgram (myLexer s) of
    Right tree -> case runTypecheckerMonad tree of
        Right _ -> do
            (result, _) <- runInterpreterMonad tree
            case result of
                Right _ ->
                    exitSuccess
                Left (pos, err) -> do
                    exitError pos $ Interpreter.Exception.getExceptionMessage err
                    exitFailure
        Left (pos, err) -> do
            exitError pos $ Typechecker.Exception.getExceptionMessage err
            exitFailure
    Left s -> exitError BNFC'NoPosition s

exitError :: BNFC'Position -> String -> IO ()
exitError pos err = do
    hPutStr stderr "error:"

    unless (isNothing pos) (let (ln, col) = fromJust pos in hPutStr stderr (show ln ++ ":" ++ show col ++ ":"))
    hPutStr stderr " "
    hPutStrLn stderr err
    exitFailure



