module Interpreter.EnvironmentUtils where

import           Grammar.Abs

import           Common.Exception
import           Interpreter.Environment
import           Interpreter.Exception
import           Interpreter.Monad

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                as Map
import           Data.Maybe

newloc :: Store -> Loc
newloc = toInteger . Map.size

find :: Ident -> InterpreterMonad MVal
find = extractLoc >=> extractMVal
updateAtIdent :: Ident -> MVal -> InterpreterMonad ()
updateAtIdent i mval = do
  l <- extractLoc i
  updateAtLoc l mval

updateAtLoc :: Loc -> MVal -> InterpreterMonad ()
updateAtLoc l mval = modify(& at l ?~ mval)

transformIntV :: Ident -> (Integer -> Integer) -> InterpreterMonad ()
transformIntV i f = do
  (IntV v) <- find i
  updateAtIdent i (IntV $ f v)

extractLoc :: Ident -> InterpreterMonad Loc
extractLoc i = do
  env <- ask
  return $ fromJust $ env ^. mem . at i

extractMVal :: Loc -> InterpreterMonad MVal
extractMVal l = do
  store <- get

  return $ fromJust $ store ^. at l

declareIdent :: Ident -> MVal -> InterpreterMonad Env
declareIdent i mval = do
  env <- ask
  store <- get

  let l = newloc store
  put (store & at l ?~ mval)
  return $ env & (mem . at i ?~ l)

flattenArgs :: [Arg] -> [FuncArg]
flattenArgs = concatMap (\(Ar _ idents _) -> map FuncArg idents)

mValType :: MVal -> Type
mValType mval = case mval of
    BoolV _    -> Bool BNFC'NoPosition
    IntV _     -> Int BNFC'NoPosition
    StringV _  -> Str BNFC'NoPosition
    PtrV _     -> undefined
    NilV       -> undefined
    FuncDefV{} -> undefined
    VoidV      -> Void BNFC'NoPosition

defaultMVal :: Type -> InterpreterMonad MVal
defaultMVal t = case t of
    Int _            -> return $ IntV 0
    Str _            -> return $ StringV ""
    Bool _           -> return $ BoolV False
    Void _           -> undefined
    Ptr _ _          -> return NilV
    FuncLitT pos _ _ -> throwPosError pos FuncLitNoDefault
    FuncLit pos _    -> throwPosError pos FuncLitNoDefault
