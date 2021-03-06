-- File generated by the BNF Converter (bnfc 2.9.3).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Instant.Skel where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified Instant.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: Instant.Abs.Ident -> Result
transIdent x = case x of
  Instant.Abs.Ident string -> failure x

transProgram :: Instant.Abs.Program -> Result
transProgram x = case x of
  Instant.Abs.Prog stmts -> failure x

transStmt :: Instant.Abs.Stmt -> Result
transStmt x = case x of
  Instant.Abs.SAss ident exp -> failure x
  Instant.Abs.SExp exp -> failure x

transExp :: Instant.Abs.Exp -> Result
transExp x = case x of
  Instant.Abs.ExpAdd exp1 exp2 -> failure x
  Instant.Abs.ExpSub exp1 exp2 -> failure x
  Instant.Abs.ExpMul exp1 exp2 -> failure x
  Instant.Abs.ExpDiv exp1 exp2 -> failure x
  Instant.Abs.ExpLit integer -> failure x
  Instant.Abs.ExpVar ident -> failure x
