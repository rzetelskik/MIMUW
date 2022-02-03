module Typechecker.Exception where

import           Grammar.Abs

import           Common.Exception
import           Common.TypeUtils

type TracedStaticTypingException = TracedException StaticTypingException

data StaticTypingException
    = AssignmentMismatch
    | Redeclared Ident
    | TypeMismatch Type Type
    | Undefined Ident
    | InvalidIndirect Type
    | InvalidOperation
    | ReturnTooMany
    | ReturnNotEnough
    | MissingReturn
    | NonBoolCondition Type
    | FuncNotCalled
    | NonRetFuncAsValue
    | CallNonFunction Ident Type
    | UntypedNil
    | MainUndeclared
    | MainWrongType

getExceptionMessage :: StaticTypingException -> String
getExceptionMessage exc = case exc of
    AssignmentMismatch             -> "assignment mismatch"
    Redeclared (Ident i)           -> i ++ " redeclared in this block"
    TypeMismatch t1 t2             -> "cannot use type " ++ showType t1 ++ " as type " ++ showType t2
    Undefined (Ident i)            -> "undefined: " ++ i
    InvalidIndirect t              -> "invalid indirect (type " ++ showType t ++ ")"
    InvalidOperation               -> "invalid operation"
    ReturnTooMany                  -> "too many arguments to return"
    ReturnNotEnough                -> "not enough arguments to return"
    MissingReturn                  -> "missing return at end of function"
    NonBoolCondition t             -> "non-bool type " ++ showType t ++ " used as condition"
    FuncNotCalled                  -> "func value not called"
    NonRetFuncAsValue              -> "non-return func application used as value"
    CallNonFunction (Ident i) t    -> "cannot call non-function " ++ i ++ " (type " ++ showType t ++ ")"
    UntypedNil                     -> "use of untyped nil"
    MainUndeclared                 -> "function main is undeclared"
    MainWrongType                  -> "func main must have no arguments and no return values"
