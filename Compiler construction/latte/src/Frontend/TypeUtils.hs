{-# LANGUAGE PatternSynonyms #-}

module Frontend.TypeUtils where

import           Latte.Abs

import           Data.List

(===) :: Type -> Type -> Bool
Int _ === Int _                         = True
Str _ === Str _                         = True
Bool _ === Bool _                       = True
Void _ === Void _                       = True
Fun _ t1 as1 === Fun _ t2 as2           = t1 === t2 && length as1 == length as2 && all (uncurry (===)) (zip as1 as2)
_ === _                                 = False

isVoid :: Type -> Bool
isVoid = (===) (Void NoPos)

showType :: Type -> String
showType t = case t of
    Int _      -> "int"
    Str _      -> "string"
    Bool _     -> "boolean"
    Void _     -> undefined
    Fun _ t as -> showType t ++ "(" ++ showTypes as ++ ")"

showTypes :: [Type] -> String
showTypes = intercalate ", " . map showType

repositionType :: Type -> BNFC'Position -> Type
repositionType t pos = case t of
    Int _      -> Int pos
    Str _      -> Str pos
    Bool _     -> Bool pos
    Void _     -> Void pos
    Fun _ t as -> Fun pos t as


pattern NoPos = BNFC'NoPosition
