{-# LANGUAGE FlexibleContexts #-}

module Frontend.Exception where

import           Control.Monad.Except
import           Latte.Abs

import           Frontend.TypeUtils

type TracedException e = (BNFC'Position, e)

type TracedSemanticAnalysisException = TracedException SemanticAnalysisException

data SemanticAnalysisException
    = Redeclared Ident
    | TypeMismatch Type Type
    | Undefined Ident
    | InvalidOperation
    | MissingReturn
    | NonVoidReturn
    | VoidReturn
    | NonBoolCondition Type
    | NonRetFuncAsAddress
    | CallBadArgNum Ident [Type] [Type]
    | CallNonFunction Ident Type
    | MainUndeclared
    | MainWrongType

getExceptionMessage :: SemanticAnalysisException -> String
getExceptionMessage exc = case exc of
    Redeclared (Ident i)              -> i ++ " redeclared in this block"
    TypeMismatch t1 t2                -> "cannot use type " ++ showType t1 ++ " as type " ++ showType t2
    Undefined (Ident i)               -> "undefined: " ++ i
    InvalidOperation                  -> "invalid operation"
    MissingReturn                     -> "missing return at end of function"
    NonVoidReturn                     -> "non-void function should return a address"
    VoidReturn                        -> "void function should not return a address"
    NonBoolCondition t                -> "non-bool type " ++ showType t ++ " used as condition"
    NonRetFuncAsAddress                 -> "non-return func application used as address"
    CallBadArgNum (Ident i) have want -> (if length have < length want then "not enough" else "too many") ++ " arguments in call to " ++ i ++ "\nhave (" ++ showTypes have ++ ")\nwant (" ++ showTypes want ++ ")"
    CallNonFunction (Ident i) t       -> "cannot call non-function " ++ i ++ " (type " ++ showType t ++ ")"
    MainUndeclared                    -> "function main is undeclared"
    MainWrongType                     -> "func main must have no arguments and return an int"

throwPosError :: MonadError (TracedException e) m => BNFC'Position -> e -> m a
throwPosError = curry throwError
