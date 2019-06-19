{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | The source language for recursive tree traversals.
--   This is a first-order language for the "closed world" scenario:
--   not integrating with a functional host language, but rather
--   genarating C code like a DSL.

module Gibbon.L1.Syntax
    (
      -- * Core types specific to L1
      Prog1, FunDef1, FunDefs1, DDef1, DDefs1, Exp1, Ty1, E1Ext(..)

    , module Gibbon.Language
    ) where

import Control.DeepSeq ( NFData )
import Data.Loc
import Data.Set as S
import GHC.Generics
import Text.PrettyPrint.GenericPretty

import Gibbon.Language
import Gibbon.Common

--------------------------------------------------------------------------------

instance FunctionTy Ty1 where
  -- | At this stage, function types are just (in , out) tuples.
  type ArrowTy Ty1 = ([Ty1] , Ty1)
  inTys = fst
  outTy = snd

-- | A convenient, default instantiation of the L1 expression type.
type Exp1 = PreExp E1Ext () Ty1

-- | An L1 program.
type Prog1 = Prog (L Exp1)

-- | Datatypes
type DDefs1 = DDefs Ty1
type DDef1  = DDef Ty1

-- | Function definition used in L1 programs.
type FunDef1 = FunDef (L Exp1)

type FunDefs1 = FunDefs (L Exp1)

-- | The type rperesentation used in L1.
type Ty1 = UrTy ()


--------------------------------------------------------------------------------

-- ^ [2019.06.16] HACK: Ideally we should only deal with location
-- arithmetic in L2. However we need this in L1 to calculate certain
-- random-access-nodes. InferLocations turns this into an appropriate
-- L2 extension. See 'mkRANs' in 'AddRAN' for an example.
--
-- Invariant: This should only appear as an RHS in a let binding.
data E1Ext l d = AddCursor Var Int
  deriving (Show, Ord, Eq, Read, Generic, Out, NFData)

instance FreeVars (E1Ext l d) where
  gFreeVars (AddCursor v _) = S.singleton v

instance Expression (E1Ext l d) where
  type TyOf  (E1Ext l d) = UrTy l
  type LocOf (E1Ext l d) = l
  isTrivial _ = False

instance Flattenable (E1Ext l d) where
  gFlattenGatherBinds  _ _ e = return ([],e)
  gFlattenExp _ _ e          = return e

instance HasSimplifiableExt E1Ext l d => SimplifiableExt (L (PreExp E1Ext l d)) (E1Ext l d) where
  gInlineTrivExt _ e = e

instance HasSubstitutableExt E1Ext l d => SubstitutableExt (L (PreExp E1Ext l d)) (E1Ext l d) where
  gSubstExt _ _ e  = e
  gSubstEExt _ _ e = e

instance Typeable (E1Ext l d) where
  gRecoverType _ _ _ = CursorTy

instance Renamable (E1Ext l d) where
  gRename env (AddCursor v i) = (AddCursor (gRename env v) i)
