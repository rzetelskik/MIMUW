{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Backend.EnvironmentUtils where

import           Backend.Environment
import           Backend.IR
import           Backend.IRUtils
import           Backend.Monad
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map             as Map
import           Data.Maybe

class HasFresh i a where
    nextFresh :: a -> (i,a)

instance HasFresh Loc IRGState where
    nextFresh s = (loc, s)
        where
            loc = Loc $ toInteger . Map.size $ (s ^. storeVars)

instance HasFresh Label IRGState where
    nextFresh s = (l, s & nextLabel .~ Label (unLabel l + 1))
        where
            l = s ^. nextLabel

instance HasFresh ConstId IRGState where
    nextFresh s = (id, s & nextConstId .~ ConstId (unConstId id + 1))
        where
            id = s ^. nextConstId

instance HasFresh Integer IRGState where
    nextFresh s = (s ^. nextLocal, s & nextLocal +~ 1)

fresh :: (HasFresh i s, MonadState s m) => m i
fresh = do
    (i,s) <- gets nextFresh
    put s
    return i

newTemp :: IRGMonad Arg
newTemp = fresh <&> AReg

emit :: Instr -> IRGMonad ()
emit i = do
    l <- use currBlock
    block <- use (blocks . at l) <&> fromJust
    modifying blocks $ at l ?~ (block & bRinstrs %~ cons i)

findIdent :: Ident -> IRGMonad (Type, Arg)
findIdent = getLoc >=> getAddress

getLoc :: Ident -> IRGMonad Loc
getLoc i = asks (^. memVars . at i) <&> fromJust

getAddress :: Loc -> IRGMonad (Type, Arg)
getAddress l = use (storeVars . at l) <&> fromJust

declareIdent :: Ident -> (Type, Arg) -> IRGMonad IRGEnv
declareIdent i arg = do
    l <- fresh
    storeVars %= (at l ?~ arg)
    asks (& memVars . at i ?~ l)

getLabel :: IRGMonad Label
getLabel = do
    l <- fresh
    blocks %= (at l ?~ Block l [])
    return l

getConstId :: String -> IRGMonad ConstId
getConstId k = do
    use (storeConsts . at k) >>= \case
        Just id ->
            return id
        Nothing -> do
            id <- fresh
            storeConsts %= (at k ?~ id)
            return id

declareSignature :: Signature -> IRGMonad ()
declareSignature signature = modifying storeSignatures $ at ident ?~ signature
    where
        ident = signature ^. sIdent

findSignature :: Ident -> IRGMonad Signature
findSignature ident = use (storeSignatures . at ident) <&> fromJust

refreshState :: IRGMonad ()
refreshState = do
    nextLabel .= Label 0
    nextLocal .= 0
    storeVars .= Map.empty
    blocks    .= Map.empty
    currBlock .= Label 0

useBlock :: Label -> IRGMonad ()
useBlock = assign currBlock

blockReturns :: Label -> IRGMonad Bool
blockReturns l = getLastInstr l <&> returns

getLastInstr :: Label -> IRGMonad (Maybe Instr)
getLastInstr l = do
    instrs <- use (blocks . at l) <&> fromJust <&> view bRinstrs
    return $ if Prelude.null instrs then Nothing else Just $ head instrs
