{-# LANGUAGE PatternSynonyms #-}

module Common.TypeUtils where

import           Grammar.Abs

(===) :: Type -> Type -> Bool
Int _ === Int _                         = True
Str _ === Str _                         = True
Bool _ === Bool _                       = True
FuncLitT _ as1 t1 === FuncLitT _ as2 t2 = FuncLit NoPos as1 === FuncLit NoPos as2 && t1 === t2
FuncLit _ as1 === FuncLit _ as2         = length as1 == length as2 && all (uncurry (===)) (zip as1 as2)
Ptr _ _ === Ptr _ (Void _)              = True
Ptr _ t1 === Ptr _ t2                   = t1 === t2
Void _ === Void _                       = True
_ === _                                 = False

showType :: Type -> String
showType t = case t of
    Int _           -> "int"
    Str _           -> "string"
    Bool _          -> "bool"
    FuncLitT _ as t -> "func(" ++ concatMap showType as ++ ") " ++ showType t
    FuncLit _ as    -> "func(" ++ concatMap showType as ++ ")"
    Ptr _ t         -> "*" ++ showType t
    Void _          -> undefined

repositionType :: Type -> BNFC'Position -> Type
repositionType t pos = case t of
    Int _           -> Int pos
    Str _           -> Str pos
    Bool _          -> Bool pos
    FuncLitT _ as t -> FuncLitT pos as t
    FuncLit _ as    -> FuncLit pos as
    Ptr _ t         -> Ptr pos t
    Void _          -> Void pos


pattern NoPos = BNFC'NoPosition
