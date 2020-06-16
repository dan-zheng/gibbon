{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Interpreter for the source language (L0)
module Gibbon.L0.Interp where

import           Control.Monad.Writer
import           Data.ByteString.Builder ( toLazyByteString )

import           Gibbon.L0.Syntax
import           Gibbon.L1.Interp ( interp )

--------------------------------------------------------------------------------

instance InterpExt Exp0 (E0Ext Ty0 Ty0) where
  gInterpExt rc valenv ddefs fundefs ex = do
    (res,logs) <- runWriterT interpExt0
    pure (res, toLazyByteString logs)
    where
      interpExt0 :: WriterT InterpLog IO (Value Exp0)
      interpExt0 =
        case ex of
          LambdaE args bod -> return (VLam (map fst args) bod valenv)
          BenchE fn locs args _b -> do
            (v, _) <- lift $ gInterpExp rc valenv ddefs fundefs (AppE fn locs args)
            return v
          ParE0 ls -> do
            (v, _) <- lift $ gInterpExp rc valenv ddefs fundefs (MkProdE ls)
            return v
          L _ e -> do
            (v, _) <- lift $ gInterpExp rc valenv ddefs fundefs e
            return v
          PolyAppE{} -> error "L0.Interp: PolyAppE not handled."
          FunRefE{} -> error "L0.Interp: FunRefE not handled."

instance Interp Exp0 where
  gInterpExp rc valenv ddefs fundefs e = do
    (res,logs) <- runWriterT (interp rc valenv ddefs fundefs e)
    pure (res, toLazyByteString logs)
