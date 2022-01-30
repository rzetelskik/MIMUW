{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Backend.Environment where

import           Backend.IR
import           Control.Lens
import qualified Data.Map     as Map

newtype Loc = Loc { unLoc :: Integer } deriving (Eq, Ord)

newtype IRGEnv = IRGEnv { _memVars :: Map.Map Ident Loc }

initEnv :: IRGEnv
initEnv = IRGEnv { _memVars = Map.empty }

data IRGState = IRGState {
    _nextLabel       :: Label,
    _nextLocal       :: Integer,
    _nextConstId     :: ConstId,
    _storeSignatures :: Map.Map Ident Signature,
    _storeVars       :: Map.Map Loc (Type, Arg),
    _storeConsts     :: Map.Map String ConstId,
    _blocks          :: Map.Map Label Block,
    _currBlock       :: Label
    }

initState :: IRGState
initState = IRGState {
    _nextLabel = Label 0,
    _nextLocal = 0,
    _nextConstId = ConstId 0,
    _storeSignatures = Map.empty,
    _storeVars = Map.empty,
    _storeConsts = Map.empty,
    _blocks = Map.empty,
    _currBlock = Label 0
    }

makeLenses ''IRGState
makeLenses ''IRGEnv
