{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Grisette.Lib.Control.Carrier.Fresh.Church where

import Control.Carrier.Fresh.Church
import Grisette.Core
import Grisette.Lib.Control.Carrier.State.Church

instance
  (SymBoolOp bool, GUnionLike bool m, GMergeable bool a) =>
  GMergeable bool (FreshC m a)
  where
  gmergingStrategy = gwrapStrategy gmergingStrategy1 FreshC (\(FreshC et) -> et)

instance (SymBoolOp bool, GUnionLike bool m, Functor m) => GMergeable1 bool (FreshC m) where
  liftGMergingStrategy ms = gwrapStrategy (liftGMergingStrategy ms) FreshC (\(FreshC et) -> et)

instance
  (SymBoolOp bool, GUnionLike bool m, GMergeable bool a, Functor m) =>
  GSimpleMergeable bool (FreshC m a)
  where
  gmrgIte cond (FreshC t) (FreshC f) = FreshC $ gmrgIte cond t f

instance
  (SymBoolOp bool, GUnionLike bool m, Functor m) =>
  GSimpleMergeable1 bool (FreshC m)
  where
  liftGMrgIte s = mrgIfWithStrategy (SimpleStrategy s)

instance
  (SymBoolOp bool, GUnionLike bool m, Functor m) =>
  GUnionLike bool (FreshC m)
  where
  mergeWithStrategy s (FreshC et) = FreshC $ mergeWithStrategy s et
  mrgIfWithStrategy s cond (FreshC l) (FreshC r) = FreshC $ mrgIfWithStrategy s cond l r
  mrgSingleWithStrategy ms a = FreshC $ mrgSingleWithStrategy ms a
  single a = FreshC $ single a
  unionIf cond (FreshC t) (FreshC f) = FreshC $ unionIf cond t f

mrgRunFresh ::
  (SymBoolOp bool, GUnionLike bool m, GMergeable bool b) =>
  (Int -> a -> m b) ->
  Int ->
  FreshC m a ->
  m b
mrgRunFresh k n (FreshC m) = mrgRunState k n m

mrgEvalFresh ::
  (SymBoolOp bool, GUnionLike bool m, GMergeable bool a, Functor m) =>
  Int ->
  FreshC m a ->
  m a
mrgEvalFresh n (FreshC m) = mrgEvalState n m
