module Interpreter.Exception where

import           Grammar.Abs

import           Common.Exception
import           Interpreter.Environment

type TracedRuntimeException = TracedException RuntimeException

data RuntimeException
    = DivisionByZero
    | CantTakeAddr
    | FuncLitNoDefault
    | NilDereference
    | Return MVal
    deriving Show

getExceptionMessage :: RuntimeException -> String
getExceptionMessage exc = case exc of
    DivisionByZero   -> "division by zero"
    CantTakeAddr     -> "cannot take the address"
    FuncLitNoDefault -> "cannot create an uninitialised function"
    NilDereference   -> "nil dereference"
    Return _         -> undefined
